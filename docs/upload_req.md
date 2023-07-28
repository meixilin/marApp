When uploading your custom files for MAR calculations, 
please ensure that the files follow the formatting guidelines below:

1. Files should be tab-delimited and end with `*.txt` or `*.tsv` or `*.gz`.
2. Gzipped files are allowed. For genotype files, we encourage you gzip the files before uploading. 

**Coordinate file**

1. Three columns: `ID`,`LON`,`LAT` 

Example file with 5 samples:

```
ID	LON	LAT
1	9.05	48.52
2	9.04	48.53
3	25.74	44.46
4	21.95	46.11
5	34.3814	44.6419
```

**Genotype file**

1. Rows are samples and columns are `ID` + genotypes. 
    * The first column is `ID`, should be the same as the first column in coordinate file.  
    * The second to last columns are genotypes, the column names can be any values that denote the SNP id. 
2. Other than the first column (`ID`), the genotype columns should take values of `0/1/2` (in diploid organisms).
    * `0/1/2` represents the number of alternative alleles for each genotype. 
    
Example file with 5 samples and 4 SNPs: 

```
ID      SNP.1   SNP.2   SNP.3   SNP.4
1       0       2       0       0
2       0       0       0       0
3       0       0       1       0
4       0       0       0       0
5       2       1       0       0
```


