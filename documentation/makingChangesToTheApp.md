# Guide to Making Changes to the GitHub Repository

This guide outlines the process for making changes to the repository. Since the `main` branch is used for deployment, all changes must be made and tested on a new branch before being merged.

## 1. Cloning the Repository

If you haven't already cloned the repository, run:

```sh
 git clone https://github.com/EnvironmentalWebApplication/RShinyApp
 cd RShinyApp
```

## 2. Creating a New Branch

Before making any changes, create a new branch off `main`:

```sh
git checkout main

git pull origin main  # Ensure you have the latest changes

git checkout -b <new-branch-name>
```

Use a descriptive branch name, e.g., `feature-add-login`, `bugfix-fix-header`, etc.

## 3. Making Changes

Edit the necessary files and test your changes locally.

Commit often to save your progress and ensure that changes are well-documented. Frequent commits make it easier to track changes and revert if needed.

## 4. Committing Changes

After making changes, stage and commit them:

```sh
git add .  # Or specify individual files: git add <file-name>

git commit -m "Your descriptive commit message"
```

## 5. Pushing the Branch

Push your branch to GitHub:

```sh
git push origin <new-branch-name>
```

## 6. Creating a Pull Request (PR) on GitHub

1. Go to the repository on **GitHub.com**.
2. Click on **Pull Requests** > **New Pull Request**.
3. Select `main` as the base branch and your new branch as the compare branch.
4. Add a descriptive title and description of the changes.
5. Request reviews from relevant team members.
6. Submit the pull request.

## 7. Merging to Main

Once approved:

```sh
git checkout main

git pull origin main  # Ensure it's up to date

git merge <new-branch-name>

git push origin main
```

Alternatively, use the **Merge** button on GitHub after approval.

## 8. Cleaning Up

After merging, delete the branch:

```sh
git branch -d <new-branch-name>
git push origin --delete <new-branch-name>
```

## 9. Verifying Deployment

Monitor the deployment process and verify that the changes work as expected.

---

**Best Practices:**
- Keep changes focused and small.
- Write meaningful commit messages.
- Commit often to save work and track progress.
- Always test before pushing changes.
- Ensure no sensitive information is committed.
- Keep your local `main` branch up to date by pulling frequently.
