$ErrorActionPreference = "Stop"

$LLVMVersion = Get-Content -Path (Join-Path $PSScriptRoot "../LLVM_VERSION") -TotalCount 1 | ForEach-Object { $_.Trim() }

$BaseUrl = "https://github.com/llvm/llvm-project/releases/download/llvmorg-${LLVMVersion}"
$Arch = $env:PROCESSOR_ARCHITECTURE
$Filename = ""

if ($Arch -eq "AMD64") {
    $Filename = "LLVM-${LLVMVersion}-win64.exe"
} elseif ($Arch -eq "ARM64") {
    $Filename = "LLVM-${LLVMVersion}-woa64.exe"
} else {
    Write-Error "Unsupported architecture: $Arch"
    exit 1
}

$LLVMPrefix = Join-Path $env:RUNNER_TEMP "llvm-download"

Invoke-WebRequest -Uri "${BaseUrl}/${Filename}" -OutFile $Filename
Start-Process -FilePath $Filename -ArgumentList "/S", "/D=$LLVMPrefix" -Wait -NoNewWindow

$LLVMVersionSuffix = $LLVMVersion.Split('.')[0] + "0"
"TABLEGEN_${LLVMVersionSuffix}_PREFIX=${LLVMPrefix}" | Out-File -FilePath $env:GITHUB_ENV -Append -Encoding utf8
