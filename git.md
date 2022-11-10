# Git

Before committing: `git status`, `git diff`

Adding all: `git add -A`

Diff after commit: `git diff --staged`

Bypass checks: `git commit --no-verify -m "commit message"`

Pushing the first time: `git push -u origin master`

Viewing changes in all commits: `git log -p` (patch)

Viewing changes in a single file: `git log -p -- filename`

Viewing changes in conflict resolution: `git log -p -c -- filename`

Viewing changes in last commit: `git log -p -1`

Viewing changes in a specific commit: `git show 2ef3abc` (first few characters of hash, at least 4 chars)

Show in one line whether file was added, modified, renamed or deleted (useful when there are long lines) `git log --raw`

Show filenames only in log: `git log --name-only`

New branch: `git checkout -b new_feature`

List branches: `git branch -a`

Compare branches: `git diff brancha..branchb`

Compare file status between branches: `git diff --name-status brancha..branchb`

Compare file between branches: `git diff brancha..branchb -- myfile.txt`

Compare current branch with master: `git diff master`

To avoid Git from copying the whole repo after branching,
run right before the push: `git fetch origin master`

Delete local branch: `git branch --delete my_branch`
Force delete local branch: `git branch -D my_branch`

Delete remote branch: `git push origin --delete my_branch`

Rename local branch: `git branch -m new-name`

Cannot rename remote branch. Push the new branch, then delete the old one.

`git fetch origin master`
`git push -u origin new-name`
`git push origin --delete old-name`

Show commit hashes (short): `git log --oneline`

Show commits with timestamps: `git log --pretty=format:"%h%x09%as%x09%s"`
(Aliased as githist)

Go back to a previous commit: `git checkout f5c0d32`

To move to latest commit: `git checkout -`

Revert non-committed changes: `git restore my_file`

Revert all files: `git restore .`

Unstage: `git restore --staged my_file`

To reset master to be the same as remote:

`git reset --hard origin/master`

To make a local copy of a remote branch:

`git fetch --all`
`git checkout branch_name`

To copy a file from another branch (merge single file):

`git checkout other_branch_name path/to/file_name`

Log: `git log --oneline`

To save credentials in a cache for 8 hours (28800 seconds)

`git config --global credential.helper 'cache --timeout=28800'`

To clear the credential cache:

`git credential-cache exit`

To show a pretty graph with branches

`git log --all --decorate --oneline --graph` (aliased as gitgraph)

To get a local copy of all branches:

```
mkdir backup_repo
cd backup_repo
git clone --bare https://path/to/repo.git .git
git config --unset core.bare
git reset --hard
```

To merge two files (without branches)

`git merge-file currentFileToBeWritten originalFile fileToPullChangesFrom`

To compare two files in the same branch

`git diff --no-index FILE_A FILE_B`

Clean up local view of branches that have been deleted remotely:

`git fetch -p`

Cancel a merge with conflicts

`git merge --abort`

Check if dev branch has been merged into master:

`git log master..dev`

if nothing is printed, dev has been merged into master.

Remove references to deleted remote branches

`git remote update origin --prune`

View history of a portion of a file

`git log -L 1,99:File.txt`

Revert latest commit

`git revert <hash of commit to undo>`

Reset to given commit

Do not use if others have pulled your code

`git reset --hard <previous commit>`
`git push -f origin <branch name>`

Stash with a name (recommended)

`git stash push -m "name of stash"

To apply a stash:

First, check its index, then apply or pop by its index number

`git stash list`
`git stash apply 3`
`git stash pop 3`