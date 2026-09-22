<#
Ionide's F# language server (fsautocomplete) keeps this solution's output DLLs open for
IntelliSense, which makes `dotnet build`/`dotnet test` fail with MSB3027 file-lock errors
while VS Code is open. This script stops fsautocomplete, runs the requested dotnet command,
then waits for VS Code's language client to relaunch it - which it does on its own whenever
the server process disappears unexpectedly, success or failure, so the wait always runs.
#>
param(
    [ValidateSet("build", "test")]
    [string]$Mode = "test"
)

function Get-FsacProcess {
    Get-CimInstance Win32_Process -Filter "Name = 'dotnet.exe'" |
        Where-Object { $_.CommandLine -like "*fsautocomplete.dll*" }
}

$fsac = Get-FsacProcess
if ($fsac) {
    Write-Host "Stopping fsautocomplete (PID $($fsac.ProcessId)) to release file locks..."
    Stop-Process -Id $fsac.ProcessId -Force -ErrorAction SilentlyContinue
    Start-Sleep -Milliseconds 500
} else {
    Write-Host "fsautocomplete isn't running; nothing to stop."
}

try {
    if ($Mode -eq "build") {
        dotnet build
    } else {
        dotnet test
    }
    $exitCode = $LASTEXITCODE
} finally {
    Write-Host "Waiting for VS Code to relaunch fsautocomplete..."
    $relaunched = $false
    for ($i = 0; $i -lt 20; $i++) {
        Start-Sleep -Milliseconds 500
        if (Get-FsacProcess) { $relaunched = $true; break }
    }
    if ($relaunched) {
        Write-Host "fsautocomplete is back up."
    } else {
        Write-Host "fsautocomplete didn't come back on its own - run 'Developer: Reload Window' or 'F#: Restart Language Server' in VS Code."
    }
}

exit $exitCode
