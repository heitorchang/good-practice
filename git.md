# Git

Before committing: `git status`, `git diff`

Adding all: `git add -A`

Diff after commit: `git diff --staged`

Bypass checks: `git commit --no-verify -m "commit message"`

Pushing the first time: `git push -u origin master`

New branch: `git checkout -b new_feature`

List branches: `git branch -a`

To avoid Git from copying the whole repo after branching,
run right before the push: `git fetch origin master`

Delete local branch: `git branch -d my_branch`

Delete remote branch: `git push origin --delete my_branch`

Revert non-committed changes: `git restore my_file`

Unstage: `git restore --staged my_file`

Log: `git log --oneline`
