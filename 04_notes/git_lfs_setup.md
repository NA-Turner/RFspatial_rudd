# Git LFS Setup Guide


## What is Git LFS?

**Git Large File Storage (LFS)** is an extension to Git that allows you to version control large files efficiently. Instead of storing large files directly in your Git repository, LFS stores only a small pointer file in the repo while the actual file contents are stored on a separate LFS server.


## Why Use LFS?

- **Prevents repository bloat**: Large files don't inflate your `.git` folder size
- **Faster cloning**: Initial clones download pointers, not full file contents
- **Version control for large data**: Track changes to datasets, figures, and other large files
- **Automatic handling**: Once configured, LFS works transparently


## Installation

1. **Git for Windows installer**
   - Download and install from: https://git-scm.com/download/win

2. **Verify installation**
   ```bash
   git lfs version
   ```
   You should see output like: `git-lfs/3.x.x`


## One-Time Setup (Per User)

After installing Git LFS, run this command once on your machine:

```bash
git lfs install
```

This sets up the necessary Git hooks to make LFS work automatically.


## Repository Setup

### For This Template Repository

This repository is **already configured** for Git LFS via the `.gitattributes` file.

The following directory is tracked by LFS:
- `01 - Data/01 - Large Files/**` (all files in this folder)

### If Cloning This Repository

1. **Ensure Git LFS is installed** (see Installation section above)

### If Adding LFS to a New Repository

If you're using this as a template and need to set up a new repo:

1. **Initialize Git** (if not already initialized)
   ```bash
   git init
   ```

2. **Ensure `.gitattributes` exists**
   - This template includes it already
   - It specifies which files should use LFS

3. **Add and commit** your files normally
   ```bash
   git add .
   git commit -m "Initial commit with LFS tracking"
   ```

4. **Add remote and push**
   ```bash
   git remote add origin <repository-url>
   git push -u origin main
   ```

## How to Use

### Adding Large Files

Simply add files to the `01 - Data/01 - Large Files/` directory and commit normally:

Git LFS handles everything automatically - no special commands needed!


### Viewing LFS File History

```bash
# See which commits modified LFS files
git log --follow -- "01 - Data/01 - Large Files/my_file.csv"
```

## Important Notes

### File Size Limits

- **GitHub**: 2 GB per file, 1 GB free storage per account
If you exceed limits, you may need to purchase additional LFS storage or use alternative solutions.

### What NOT to Track with LFS

- **Small files** (< 1 MB): Regular Git handles these efficiently
- **Files that change frequently**: LFS stores every version, so rapidly changing files accumulate storage
- **Temporary files**: These should be in `.gitignore` instead

### What TO Track with LFS

- **Large datasets** (.csv, .rds, .parquet files)


## Troubleshooting

### "This repository is over its data quota"

Your LFS storage limit has been exceeded. Options:
1. Purchase additional storage from your Git provider
2. Remove large files from history using `git lfs prune`
3. Use external storage (cloud storage) and reference via URLs

### "Git LFS is not installed"

Run the installation steps above, then:
```bash
git lfs install
git lfs pull
```

### Files not downloading during clone

```bash
# Manually fetch LFS files
git lfs pull
```

### Checking if a file is in LFS

```bash
# Check if file is an LFS pointer
git lfs ls-files | grep "filename"

# Or examine the file directly
cat "01 - Data/01 - Large Files/myfile.csv" | head -5
```

If it shows `version https://git-lfs.github.com/spec/v1`, it's an LFS pointer.

## Additional Resources

- **Official Documentation**: https://git-lfs.github.com/
- **GitHub LFS Guide**: https://docs.github.com/en/repositories/working-with-files/managing-large-files
- **Atlassian Tutorial**: https://www.atlassian.com/git/tutorials/git-lfs

## Summary

Once Git LFS is installed and configured (one-time setup), it works quietly:

1. Add files to `01 - Data/01 - Large Files/`
2. Commit and push normally
3. Git automatically handles LFS routing
4. Collaborators automatically get large files when they clone/pull
