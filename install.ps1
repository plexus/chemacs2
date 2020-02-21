param(
    [switch] $Confirm = $True
)

$ErrorActionPreference = "Stop"

$ChemacsPath = Join-Path (Split-Path $PSCommandPath) ".emacs"

Try {
  Set-Variable -Scope global EmacsPath $(
    Join-Path $(Get-Variable -Scope global HOME).Value ".emacs"
  )

} Catch {
  # If $HOME hasn't been defined by the user, then emacs will default to this
  # directory on modern versions of Windows. Older versions of Windows probably
  # don't even have PowerShell installed, so I make no effort to support them.
  Set-Variable -Scope global EmacsPath $(
    Resolve-Path "~/AppData/roaming/.emacs"
  )
}

If (-Not (Test-Path -Path $EmacsPath)) {
    # In Windows, symlinks require admin privileges, so we copy the
    # file wholesale instead.
    Write-Host "OK copying file to ~/.emacs..."

    Copy-Item $ChemacsPath $EmacsPath
} Else {
    # Because we copied the file instead of symlinking it, we have to try to
    # detect changes with hashing instead...
    If ((Get-FileHash $ChemacsPath).Hash -Eq (Get-FileHash $EmacsPath).Hash) {
        Write-Host "OK chemacs appears to already be copied over, you're all good."
    } Else {
        Write-Host ("WARN content is different between {0} and {1}" -f @($EmacsPath, $ChemacsPath))
        
        $should_write = $False

        If ($Confirm) {
            Write-Host "WARN chemacs may already be installed OR something else may be in the way"
            $ans = Read-Host ("Do you want to overwrite {0}?" -f $EmacsPath)

            If (@("y", "yes", "Y", "YES").Contains($ans)) {
                $should_write = $True
            }
        } Else {
            $should_write = $True
        }

        If ($should_write) {
            Copy-Item $ChemacsPath $EmacsPath
            Write-Host "OK updated chemacs files successfully."
        } Else {
            Write-Host "WARN took no action."
        }
    }
}