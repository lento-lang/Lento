# Windows installer for the Lento toolchain

$FILE_URL = "https://api.github.com/repos/lento-lang/lento/releases/latest"
FILE_NAME="lento-win.zip"

write-host "Installing Lento toolchain..." -ForegroundColor Yellow

# Download the latest release
$latest = Invoke-RestMethod -Uri $FILE_URL
$downloadUrl = $latest.assets | ? { $_.name -eq $FILE_NAME } | % { $_.browser_download_url }
Invoke-WebRequest -Uri $downloadUrl -OutFile $FILE_NAME

# Create the installation directory
$installDir = "$env:USERPROFILE/lento"
New-Item -ItemType Directory -Force -Path $installDir
Move-Item -Path $FILE_NAME -Destination $installDir
Expand-Archive -Path "$installDir/$FILE_NAME" -DestinationPath $installDir
Remove-Item -Path "$installDir/$FILE_NAME"

# Add the installation directory to the PATH
$env:Path += ";$installDir"

write-host "Installation complete!" -ForegroundColor Green
write-host "Run 'lt' to get started."
