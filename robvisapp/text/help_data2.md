Once you have created your summary table, save it as one of the acceptable formats and upload it to `robvis` using the "Choose data file" box. The `robvis` app now accepts multiple file types when uploading your risk of bias assessment summary table; however, we recommend using either an Excel spreadsheet file (`.xlsx`/`.xls`), or a Comma Seperated Values (`.csv`) file. Once uploaded, a table of your data is presented for you to review. If you spot an error, clicking on a cell allows you to edit its contents.

Alternatively you can choose to enter your data directly into the app, by clicking the "enter your data manually" link below the "Choose data file" box. Again, clicking on a cell will allow you to enter your risk of bias judgements, and data validation ensures that they are valid judgements for the assessment tool you specified.

<br>

#### __Warnings and data validation__

`robvis` checks your uploaded data to make sure that there are no common mistakes present in it that will prevent the app from working. If it finds any issues, these will be presented in red text above the "Generate Plots" button, which will be disabled until the issues are fixed.

Common warning messages include:

* *WARNING: Duplicated study names*: `robvis` will not work correctly if two studies in your dataset have the exact same name. Distinguish between identify authors & year of publication by adding a/b to the end, for example, "Higgins, 2019a" and "Higgins, 2019b"
* *WARNING: Incorrect number of columns*: This frequently occurs either when: (a) you have accidentally choosen the wrong template for the tool used, or; (b) you have included the wrong number of columns in your dataset given the options you specified. See the example table presented above or the relevant Excel file on the "Home" tab for an example of what your summary table should look like.
* *WARNING: No weights found*: This occurs when you specify that your dataset contains a "Weight" column, but the last column of your uploaded dataset does not appear to contain any weights.
* *WARNING: Invalid judgement*: This error occurs when the data you have uploaded contains a risk-of-bias judgement that is not one of the valid judgements for the tool you have used (e.g. "Moderate" is incorrectly used in place of "Some concerns" when using the ROB2 tool). The warning will list the valid judgements for the tool you have used, and you can edit the incorrect cells by clicking on them. 

Once you have addressed all of the Warning messages, the "Generate Plot" button will become clickable and you will be able to proceed.