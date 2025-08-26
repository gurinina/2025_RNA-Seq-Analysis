# Student Guide (RStudio only): Notes, Codebook, and Updates

This guide shows how to **work safely in RStudio without using Terminal**. You’ll keep your copy up to date and your own notes separate so updates never conflict.

---

## What “Rproject main directory” means
Your **Rproject main directory** is the folder that contains the course `.Rproj` file. Open the project by double‑clicking that `.Rproj` file.

We’ll keep all personal work inside a folder named **`students_notes/`** at the *Rproject main directory*.

---

## One‑time setup (RStudio UI only)

1. **Create your notes folder**
   - In the **Files** pane (bottom right), click **New Folder** → name it `students_notes` → **OK**.

2. **Make your personal codebook from the template**
   - In the **Files** pane, navigate to `lessons/` and locate:  
     **`09-DGE_codebook_template.Rmd`**
   - Check the box next to that file → click **More** → **Copy To…**
   - In the dialog, choose the **`students_notes/`** folder and set the new name to:  
     **`<lastname>_09_codebook.Rmd`** (for example, `garcia_09_codebook.Rmd`) → **OK**.
   - Click the new file in `students_notes/` to open it. This is now **your** working copy.

> From now on, **work only inside `students_notes/`**. Anything you create or edit there is yours and won’t interfere with course updates.

---

## Getting instructor updates (no Terminal)

- Use the **Git** pane → click **Pull** to fetch the latest course changes.
- If you ever see files listed in the Git pane that you didn’t mean to change, click the file → **Revert** (Discard) to return to the instructor version, then **Pull** again.

> Your `students_notes/` folder is set up by the instructor to be ignored by Git. If it ever appears in the Git pane, **do not stage it**—just keep working in that folder.

---

## Everyday workflow

1. Open the project (`.Rproj` file) in the **Rproject main directory**.
2. **Git pane → Pull.**
3. Work in files **inside `students_notes/`** only (e.g., your codebook).
4. If something shows in the Git pane that you didn’t intend to change → **Revert** it.

---

## Optional: One‑click helper in the R Console (no Terminal)

If you prefer, run this **once** in the R Console to create/open your personal codebook automatically (replace `lastname` first):

```r
dir.create("students_notes", showWarnings = FALSE)
target <- file.path("students_notes", "lastname_09_codebook.Rmd")  # <- change lastname
if (!file.exists(target)) {
  file.copy("lessons/09-DGE_codebook_template.Rmd", target)
}
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  rstudioapi::navigateToFile(target)  # open it
} else {
  message("Your codebook is at: ", target)
}
```

---

## FAQ

**Q: Where do I put my notes and answers?**  
A: In `students_notes/` at the **Rproject main directory**.

**Q: Will updates overwrite my notes?**  
A: No. Your notes live in `students_notes/`, separate from course files.

**Q: I accidentally edited a course file outside `students_notes/`.**  
A: Use the **Git** pane → select the file → **Revert**. Then **Pull** to sync.

**Q: Do I need to push?**  
A: No. Students don’t push. Just **Pull** to get updates.
