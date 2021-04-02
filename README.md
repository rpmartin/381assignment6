# 381assignment6

Instructions on the installation of R, Rstudio, etc can be found here:

https://github.com/rpmartin/Rinstall

Once you have everything installed, here is how you get started on assignment 6.

1) go to  https://github.com/rpmartin/381assignment6

2) click on the green button labeled code.

3) click on the clipboard icon to copy address to clipboard (the icon is to the right of the text box.)

4) open Rstudio.

5) click on File in the top menu bar, then New Project, then Version Control, then Git.

6) Where it asks for Repository URL paste (control V) from your clipboard (this should be what you copied from step 3)

7) R chooses the name of the folder in which you will be working, but you should choose where you want this folder saved.

8) Click Create Project and Rstudio will start in this folder, and all the files you need have been cloned into this folder from github.

9) Open file assignment6.R

10) Ensure that your cursor is at the top of the file assignment6.R (the file is open in the top-left box in Rstudio).  Run the code by repeatedly hitting both control and enter. By doing so you step through the code line by line, and in the bottom left panel you see the results of each line of code.  *you will probably need to install library "knitr"  with the command `install.packages("knitr")`. DO NOT add install.packages("knitr") to your R script.  Installing packages is a one time event, whereas every time you knit your .Rmd file your .R file is run.*

11) There should be no errors (yet ;) when you run the code of assignment6.R.  If there are errors, do not proceed further!  Get in touch either by email or on the message board. Only once you have checked that there are no errors in your assignment6.R file should you open up assignment6.Rmd file.

12) In the assignment6.Rmd file enter your name and student ID at the top, and "sign" your academic integrity statement (replace name here with your name).

13) Knit your assignment6.Rmd file by hitting the knit button (there is a ball of yarn with a needle sticking out of it beside the word knit).  This is called knitting because we are "weaving" together R code (that produces graphs and tables) and prose.

14) A new window should open with your assignment, including one of "your" answers... you are on your way!

15) For written answers, simply write words under the questions in the assignment6.Rmd file.

16) For graphs, you create and name the graph in your assignment6.R file, in much the same way you have done for the previous assignments. Check frequently to make sure your assignment6.R file still runs without errors.  Only once you have made sure there are no errrors in assignment6.R save it to DISK. (your Rmd file can't utilize what isn't saved to disk e.g. the unsaved code in your editor or even objects in your current environment.)

17) Embed the objects in the assignment6.Rmd file by name, and knit to see the results.

Let me know if you have any issues, I am here to help!  Cheers,

Rich
