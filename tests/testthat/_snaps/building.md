# manualVersion() refuses a DESCRIPTION with no version

    Code
      manualVersion()
    Condition
      Error:
      ! manualVersion(): 'DESCRIPTION' has no Version field, so the manual would be titled, and its PDF archived, without a version.

# manualVersion() says where it looked when there is no DESCRIPTION

    Code
      manualVersion()
    Condition
      Error:
      ! manualVersion(): no DESCRIPTION at 'DESCRIPTION'.

# renderManual() errors when the site itself did not render

    Code
      renderManual(outputDir = "docs")
    Condition
      Error:
      ! renderManual(): no 'docs/index.html' was produced, so the site itself failed to render and there is nothing to publish.

# renderManual() does not accept a stale site from an earlier build

    Code
      renderManual(outputDir = "docs")
    Condition
      Error:
      ! renderManual(): no 'docs/index.html' was produced, so the site itself failed to render and there is nothing to publish.

