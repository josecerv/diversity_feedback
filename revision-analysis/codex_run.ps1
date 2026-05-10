<#
.SYNOPSIS
  Wrapper around `codex exec` that enforces the four safeguards from
  feedback_codex_safeguards.md so file-saves never silently fail.

.DESCRIPTION
  1. Pre-deletes the deliverable so its presence after exit is an unambiguous success signal.
  2. Tees stdout+stderr to a session log file for post-mortem.
  3. Hard timeout via Wait-Process (kills runaway runs).
  4. Post-run validation: deliverable exists, non-empty, and contains every required structural marker.

  Uses Codex's `-o/--output-last-message` flag so the deliverable is exactly the
  final assistant message (no thinking traces, no tool-call output).

.PARAMETER BriefPath
  Path to a markdown/text file containing the full prompt for Codex (read into stdin).

.PARAMETER DeliverablePath
  Path Codex will write the final assistant message to (passed as --output-last-message).
  Pre-deleted before launch.

.PARAMETER SessionLogPath
  Path to tee'd stdout+stderr stream from Codex.

.PARAMETER TimeoutSec
  Hard timeout in seconds. Default 1200 (20 min).

.PARAMETER RequiredSections
  List of literal substrings (e.g. "## BEFORE", "## AFTER") that the deliverable
  must contain. Wrapper exits non-zero if any are missing.

.PARAMETER WorkingDir
  Codex working directory. Default: current working directory.

.PARAMETER Sandbox
  Codex sandbox mode. Default workspace-write. Pass read-only for review-only briefs.

.PARAMETER ExtraArgs
  Additional flags appended verbatim to `codex exec`.

.EXAMPLE
  .\codex_run.ps1 -BriefPath .\briefs\study_3a.brief.md `
    -DeliverablePath .\briefs\study_3a.draft.md `
    -SessionLogPath .\briefs\study_3a.session.log `
    -RequiredSections '## BEFORE','## AFTER','## Word count:','## Preservation notes'

.NOTES
  Exit codes:
    0  success
    2  brief missing or empty
    3  codex CLI not found
    4  timeout (process killed)
    5  deliverable not written
    6  deliverable empty
    7  required section missing
    8  codex non-zero exit (deliverable still validated separately)
#>

[CmdletBinding()]
param(
    [Parameter(Mandatory)][string]$BriefPath,
    [Parameter(Mandatory)][string]$DeliverablePath,
    [Parameter(Mandatory)][string]$SessionLogPath,
    [int]$TimeoutSec = 1200,
    [string[]]$RequiredSections = @(),
    [string]$WorkingDir = (Get-Location).Path,
    [ValidateSet('read-only','workspace-write','danger-full-access')][string]$Sandbox = 'workspace-write',
    [string[]]$ExtraArgs = @()
)

$ErrorActionPreference = 'Stop'

function Append-Log([string]$line) {
    Add-Content -Path $SessionLogPath -Value $line -Encoding utf8
}

function Fail([int]$code, [string]$msg) {
    $stamp = (Get-Date -Format o)
    Append-Log "[$stamp] [FAIL code=$code] $msg"
    Write-Error $msg
    exit $code
}

# 0. Resolve / validate brief.
if (-not (Test-Path -LiteralPath $BriefPath)) { Write-Error "Brief not found: $BriefPath"; exit 2 }
$briefAbs = (Resolve-Path -LiteralPath $BriefPath).ProviderPath
$prompt = Get-Content -LiteralPath $briefAbs -Raw
if ([string]::IsNullOrWhiteSpace($prompt)) { Write-Error "Brief is empty: $briefAbs"; exit 2 }

# 0b. Verify codex CLI is on PATH.
$codexCmd = Get-Command codex -ErrorAction SilentlyContinue
if (-not $codexCmd) { Write-Error 'codex CLI not found on PATH. Install with `npm install -g @openai/codex`.'; exit 3 }

# 1. Ensure deliverable + log directories exist; pre-delete deliverable.
foreach ($p in @($DeliverablePath, $SessionLogPath)) {
    $d = Split-Path -Parent $p
    if ($d -and -not (Test-Path -LiteralPath $d)) {
        New-Item -ItemType Directory -Path $d -Force | Out-Null
    }
}
if (Test-Path -LiteralPath $DeliverablePath) {
    Remove-Item -LiteralPath $DeliverablePath -Force
}

# 2. Init session log.
$startStamp = (Get-Date -Format o)
@"
=== codex_run.ps1 start $startStamp ===
Brief:        $briefAbs
Deliverable:  $DeliverablePath
SessionLog:   $SessionLogPath
TimeoutSec:   $TimeoutSec
WorkingDir:   $WorkingDir
Sandbox:      $Sandbox
Required:     $($RequiredSections -join ' | ')
ExtraArgs:    $($ExtraArgs -join ' ')
codex CLI:    $($codexCmd.Source)
"@ | Out-File -FilePath $SessionLogPath -Encoding utf8

# 3. Build codex args. --output-last-message captures the final assistant
#    message into our deliverable file; stdout/stderr (Codex's progress
#    stream) goes to the session log via Tee-Object below.
$codexArgs = @(
    'exec',
    '--sandbox', $Sandbox,
    '--cd', $WorkingDir,
    '--skip-git-repo-check',
    '--color', 'never',
    '--output-last-message', $DeliverablePath
) + $ExtraArgs

Append-Log "---- codex argv ----"
Append-Log ("codex " + ($codexArgs -join ' '))
Append-Log "--------------------"

# 4. Run via Start-Job so we can enforce a hard timeout.
$promptFile = [System.IO.Path]::GetTempFileName()
Set-Content -LiteralPath $promptFile -Value $prompt -Encoding utf8

$job = Start-Job -ScriptBlock {
    param($CodexExe, $CodexArgs, $PromptFile, $WorkDir)
    Set-Location $WorkDir
    $ErrorActionPreference = 'Continue'
    # Pipe prompt file as stdin to codex; merge stderr with stdout (2>&1)
    Get-Content -LiteralPath $PromptFile -Raw | & $CodexExe @CodexArgs 2>&1
    return @{ ExitCode = $LASTEXITCODE }
} -ArgumentList $codexCmd.Source, $codexArgs, $promptFile, $WorkingDir

$completed = Wait-Job -Job $job -Timeout $TimeoutSec
$exitCode = $null
if ($null -eq $completed) {
    # Timeout: stop the job, kill any child codex.exe, fail.
    try { Stop-Job -Job $job -ErrorAction SilentlyContinue } catch {}
    try { Get-Process -Name codex -ErrorAction SilentlyContinue | Stop-Process -Force -ErrorAction SilentlyContinue } catch {}
    try { Remove-Job -Job $job -Force -ErrorAction SilentlyContinue } catch {}
    Remove-Item -LiteralPath $promptFile -Force -ErrorAction SilentlyContinue
    Fail 4 "Codex run timed out after $TimeoutSec sec; job stopped, codex process killed."
}

# Drain output (last item is the exit-code hashtable)
$output = Receive-Job -Job $job -Keep -ErrorAction SilentlyContinue
$lines = New-Object System.Collections.Generic.List[string]
foreach ($item in $output) {
    if ($item -is [hashtable] -and $item.ContainsKey('ExitCode')) {
        $exitCode = [int]$item.ExitCode
    } elseif ($null -ne $item) {
        $lines.Add([string]$item)
    }
}
foreach ($l in $lines) { Append-Log "[codex] $l" }
Remove-Job -Job $job -Force
Remove-Item -LiteralPath $promptFile -Force -ErrorAction SilentlyContinue

if ($null -eq $exitCode) { $exitCode = -1 }
Append-Log "=== codex exit code: $exitCode ==="

# 5. Validate deliverable.
if (-not (Test-Path -LiteralPath $DeliverablePath)) {
    Fail 5 "Deliverable not written: $DeliverablePath (codex exit $exitCode)"
}
$delBytes = (Get-Item -LiteralPath $DeliverablePath).Length
$delContent = Get-Content -LiteralPath $DeliverablePath -Raw
if ($delBytes -le 0 -or [string]::IsNullOrWhiteSpace($delContent)) {
    Fail 6 "Deliverable empty: $DeliverablePath (codex exit $exitCode, $delBytes bytes)"
}
$missing = @()
foreach ($section in $RequiredSections) {
    if ($delContent -notmatch [regex]::Escape($section)) { $missing += $section }
}
if ($missing.Count -gt 0) {
    Fail 7 "Deliverable missing required sections: $($missing -join ' | ')"
}

if ($exitCode -ne 0) {
    Append-Log "[WARN] codex returned non-zero exit $exitCode but deliverable validated."
    Write-Warning "codex exit $exitCode but deliverable looks complete; review session log: $SessionLogPath"
    exit 8
}

$endStamp = (Get-Date -Format o)
Append-Log "=== codex_run.ps1 end $endStamp ==="
Append-Log "[OK] Deliverable validated. Bytes: $delBytes. Sections found: $($RequiredSections.Count)."
Write-Host "[OK] $DeliverablePath ($delBytes bytes, $($RequiredSections.Count) sections)"
exit 0
