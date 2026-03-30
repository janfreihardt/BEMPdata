# Redirect the BEMPdata cache to a temporary directory for all tests so that
# downloaded files are automatically cleaned up when the session ends and do
# not accumulate on CI / CRAN check servers.
Sys.setenv(BEMPDATADIR = file.path(tempdir(), "BEMPdata_tests"))
