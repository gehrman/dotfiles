# The strategy here should probably to iterate through the file list,
# changing _ -> ., generating a diff, saving the diff to an install log, and
# then using patch to apply the diff. This should allow for uninstallation of
# problematic revisions. (I think.) Investigate.

pushd `dirname $0` > /dev/null
cd common
BASE_DIR=`pwd`

for base_dir in `find . -mindepth 1 -type d`; do
    target_dir=~/.${dir:3}
    if [ ! -e $target_dir ]; then
        echo Creating $target_dir directory.
        mkdir -p $target_dir
        echo
    fi
done

for file in `find . -mindepth 1 -type f`; do
    # _file --> .file
    echo Linking ${file:2} to "~/.${file:3}".
    #pwd
    ln -s ${BASE_DIR}/${file:2} ~/.${file:3}
    echo
done

popd > /dev/null
