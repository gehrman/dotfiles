# The strategy here should probably to iterate through the file list,
# changing _ -> ., generating a diff, saving the diff to an install log, and
# then using patch to apply the diff. This should allow for uninstallation of
# problematic revisions. (I think.) Investigate.
