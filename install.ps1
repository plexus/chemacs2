param(
    [switch] $Confirm = $True
)

$ErrorActionPreference = "Stop"

$ChemacsPath = Split-Path $PSCommandPath

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

function Get-Hashes {
    param(
        [string] $Directory
    )
    $Hashes = @{}

    Get-ChildItem -Recurse $Directory | ForEach-Object {
      $relative = $_.FullName.Substring($Directory.length)
      $Hashes[$relative] = (Get-FileHash $_.FullName).Hash
    }

    $Hashes
}

function Get-Intersection {
    param(
      [hashtable] $Left,
      [hashtable] $Right
    )

    $elements = @()

    ForEach ($elem in $Left.Keys) {
        If ($Right.ContainsKey($elem)) {
            $elements += $elem
        }
    }

    $elements
}

function Get-Difference {
    param(
      [hashtable] $Left,
      [hashtable] $Right
    )

    $elements = @()

    ForEach ($elem in $Left.Keys) {
        If (-Not $Right.ContainsKey($elem)) {
            $elements += $elem
        }
    }

    $elements
}

class Comparison {
  [string] $file
  [string] $SourceFile
  [string] $DestinationFile
  [string] $Status
}

function Compare-Directories {
    param(
        [string]$SourceDirectory,
        [string]$DestinationDirectory,
        [switch]$Hidden = $False
    )

    $SourceHashes = Get-Hashes -Directory $SourceDirectory -Hidden:$Hidden
    $DestinationHashes = Get-Hashes -Directory $DestinationDirectory -Hidden:$Hidden

    # Files that are in both directories and may have gotten updates
    $SharedFiles = Get-Intersection -Left $SourceHashes -Right $DestinationHashes
    # Potentially files that are "new" in the source directory
    $NewFiles = Get-Difference -Left $SourceHashes -Right $DestinationHashes
    # Potentially files that got deleted in the source directory
    $OldFiles = Get-Difference -Left $DestinationHashes -Right $SourceHashes
    
    $results = @()

    ForEach ($file in $SharedFiles) {
      $comparison = [Comparison]::new()
      $comparison.File = $file
      $comparison.SourceFile = Join-Path $SourceDirectory $file
      $comparison.DestinationFile = Join-Path $DestinationDirectory $file
      If ($SourceHashes[$file] -Eq $DestinationHashes[$file]) {
        $comparison.Status = "current"
      } Else {
        $comparison.Status = "updated"
      }

      $results += $comparison
    }

    ForEach ($file in $NewFiles) {
      $comparison = [Comparison]::new()
      $comparison.File = $file
      $comparison.SourceFile = Join-Path $SourceDirectory $file
      $comparison.DestinationFile = Join-Path $DestinationDirectory $file
      $comparison.Status = "new"

      $results += $comparison
    }

    ForEach ($file in $OldFiles) {
        $comparison = [Comparison]::new()
        $comparison.File = $file
        $comparison.SourceFile = Join-Path $SourceDirectory $file
        $comparison.DestinationFile = Join-Path $DestinationDirectory $file
        $comparison.Status = "deleted"

      $results += $comparison
    }

    $results
}

If (-Not (Test-Path -Path $EmacsPath)) {
    # In Windows, symlinks require admin privileges, so we copy the
    # directory wholesale instead.
    Write-Host "OK copying files into ~/.emacs..."

    New-Item -ItemType Directory -Path $EmacsPath | Out-Null

    # The .git folder should be hidden and therefore not copied in this
    # action. Copy-Item -Recurse naively picks those up, so we have to be a
    # little more clever.
    Get-ChildItem $ChemacsPath -Recurse | ForEach-Object {
      $dest = Join-Path $EmacsPath ($_.FullName.Substring($ChemacsPath.length))
      Copy-Item $_.FullName $dest
      
    }
} Else {
    # Because we copied the directory, we have to attempt to be smart about
    # detecting changes, so we do *this* song and dance
    $comparisons = Compare-Directories -SourceDirectory $ChemacsPath -DestinationDirectory $EmacsPath

    If (($comparisons | ForEach-Object { $_.Status -Eq "current" }) -contains $False) {
        Write-Host ("WARN content is different between {0} and {1}:" -f @($EmacsPath, $ChemacsPath))
        $comparisons | ForEach-Object {
            Write-Host ("WARN {0} - {1}" -f @($_.File, $_.Status))
        }

        $should_write = $False

        If ($Confirm) {
            Write-Host "WARN chemacs may be installed OR something else may be in the way"
            $ans = Read-Host ("Do you want to rewrite the {0} directory?" -f $EmacsPath)

            If (@("y", "yes", "Y", "YES").Contains($ans)) {
                $should_write = $True
            }
        } Else {
            $should_write = $True
        }

        If ($should_write) {
            $comparisons | ForEach-Object {
                $status = $_.Status
                $src = $_.SourceFile
                $dest = $_.DestinationFile
                Switch ($_.Status) {
                    "new" {}
                    "updated" {
                        Copy-Item $src $dest | Out-Null
                        Break;
                    }
                    "deleted" {
                        Remove-Item $dest | Out-Null
                        Break;
                    }
                }
            } | Out-Null
            Write-Host "OK updated chemacs files successfully."
        } Else {
            Write-Host "WARN took no action."
        }
    } Else {
        Write-Host "OK chemacs appears to already be copied over, you're all good."
    }
}