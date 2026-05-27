$ErrorActionPreference = "Stop"
Set-Location -LiteralPath (Split-Path -Parent $PSScriptRoot)
$rscript = (Get-Command Rscript -ErrorAction Stop).Source
& $rscript "scripts\run_shiny.R" "3840" "127.0.0.1" *> "shiny.all.log"
