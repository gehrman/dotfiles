/** Synchronize Emacs config files

Usage:
  $ sync-emacs-file path/to/source path/to/target

Specificatation:
  Prepare Phase
  * Canonicalize & make absolute source & target

  Listing Phase
  * Starting with source, maintain a queue of inputs to list contents and add to
    the queue of files to install

  Install Phase
  * Link top-level files
  * For each package, link all files in package

  Cleanup Phase
  * For now, we just print the extra contents. Since emacs uses the
**/
use std::{
    collections::{BTreeSet, VecDeque},
    env, fs,
    os::unix::fs::symlink,
    path::PathBuf,
};

fn main() -> std::io::Result<()> {
    // Prepare Phase
    let source_path = env::home_dir()
        .expect("could not get home dir")
        .join(".local/config/dotfiles/common/_emacs.d");
    let target_path = env::home_dir()
        .expect("could not get home dir")
        .join(".emacs.d");

    println!(
        "installing files from {} to {}",
        source_path.display(),
        target_path.display()
    );

    let mut input_paths = VecDeque::from([fs::canonicalize(&source_path)?]);

    // We need to to track the output dirs and files separately, as we'll first
    // need to create the output dirs, then link the output files.
    let mut output_dir_paths = VecDeque::new();
    let mut output_file_pairs = VecDeque::new();
    let mut existing_output_contents = BTreeSet::new();

    // Input Listing Phase
    while !input_paths.is_empty() {
        let path_buf = input_paths.pop_front().expect("listing underflow");

        if path_buf.is_dir() {
            let new = target_path.clone().join(
                path_buf
                    .strip_prefix(source_path.clone())
                    .expect("unable to change base"),
            );
            // println!("dir to create: {}", new.display());
            output_dir_paths.push_back(new);

            for entry_result in fs::read_dir(path_buf)? {
                input_paths.push_back(
                    entry_result
                        .expect("could not unroll directory entry")
                        .path(),
                );
            }
        } else if path_buf.is_file() {
            let new = target_path.clone().join(
                path_buf
                    .strip_prefix(source_path.clone())
                    .expect("unable to change base"),
            );
            // println!("file to link: {}", new.display());
            output_file_pairs.push_back((path_buf, new));
        } else {
            println!("found a file of unknown type: {:?}, skipping", path_buf);
        }
    }

    // Both directory creation and file linking are vulnerable to the
    // file-check race. However, this script is will be run infrequently and
    // interactively under user supervision, so if the race does happen it is
    // not hard to resolve, so we're just not worrying about it.

    // Output Directory Creation
    //   In addition to ensuring each directory exists, we also list their
    //   contents at this point. We store these in a set, so that we check there
    //   are no unexpected files present -- for now, we'll leave it up to the
    //   user to deal with extra contents.
    for dir in output_dir_paths {
        if existing_output_contents.contains(&dir) {
            existing_output_contents.remove(&dir);
        }
        if !dir.exists() {
            println!("creating {}", dir.display());
            fs::create_dir_all(dir)?;
        } else {
            for entry_result in fs::read_dir(&dir)? {
                existing_output_contents.insert(
                    entry_result
                        .expect("could not unroll directory entry")
                        .path(),
                );
            }
        }
    }
    // Output File Linking
    for (source, target) in output_file_pairs {
        if existing_output_contents.contains(&target) {
            existing_output_contents.remove(&target);
        }

        if !target.exists() {
            symlink(source, target)?;
        } else if !target.is_symlink() {
            println!(
                "--> Warning: found {} but it is not a symlink",
                target.display()
            );
        }
    }

    println!("other contents:");
    for path in existing_output_contents {
        println!("\t{}", path.display());
    }

    Ok(())
}
