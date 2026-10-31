# installModulePkgs() stops when a module's requirement is unmet

    Code
      installModulePkgs("modules", dependencies = FALSE)
    Condition
      Error:
      ! installModulePkgs(): the modules require package versions that are not installed:
        notAnInstalledPackage (>= 1.0): not installed
      Require::Install() reports this as a warning and returns, so the manual would otherwise knit against what is on disk and fail inside a module's metadata, where the message names a missing object rather than a stale package. On CI the usual cause is a restored library cache older than the modules.

