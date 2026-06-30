## R CMD check results

0 errors | 0 warnings | 0 notes

* This is a new release.

## Re-submission Responses

If there are references describing the methods in your package, please
add these in the description field of your DESCRIPTION file in the form
authors (year) <doi:...>
authors (year, ISBN:...)
or if those are not available: <https:...>
with no space after 'doi:', 'https:' and angle brackets for
auto-linking. (If you want to add a title as well please put it in
quotes: "Title")
For more details:
<https://contributor.r-project.org/cran-cookbook/description_issues.html#references>

There are no published references describing the methods in this package.

Please ensure that your functions do not write by default or in your
examples/vignettes/tests in the user's home filespace (including the
package directory and getwd()). This is not allowed by CRAN policies.
Please omit any default path in writing functions. In your
examples/vignettes/tests you can write to tempdir().
For more details:
<https://contributor.r-project.org/cran-cookbook/code_issues.html#writing-files-and-directories-to-the-home-filespace>

The default `data_dir` parameter to the `sync_server()` function now points to a temp file and not `getwd()`.