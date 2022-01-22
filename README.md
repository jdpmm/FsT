# For Statistics Tran

This project has been created to learn a little bit of Fortran and Perl programming languages, the project is about a descriptive statistics solver.
To try it, you must have a file which represent a column, for example:

```
1
2
7
4
4
2
6
...
2
2
1
```

And...
```console
$ perl run.pl filename
```

Fortran will put the result in a new file called "solved" and the struct of the table is:
```
xi, fi, Fi, hi, xi*fi, |xi - x| * fi, xi² * fi
```
The single values are:
```
Mean (x)
Mode
Median
DM
Varianza
DT
```

Fortran is awesome!!, I gotta try another project/practice with Perl :D
