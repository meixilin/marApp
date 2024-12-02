When uploading your custom files for MAR calculations,
please ensure that the files follow the formatting guidelines below:

## Coordinate file

1. Files should be tab-delimited or comma-delimited.
2. Allowed file extensions are ".txt", ".txt.gz", ".csv", ".csv.gz", ".tsv", ".tsv.gz".
3. The first line should consist of three columns: `ID`,`LON`,`LAT`.
    1. If VCF genotype file is provided, the `ID` column should match the sample IDs in the VCF file.
    2. If text genotype file is provided, the `ID` column should be sequential integers starting from 1. Please make sure that the samples are in the same order as the genotype file.
4. No missing values allowed.

Example file with 5 samples:

```
ID	LON	LAT
1	9.05	48.52
2	9.04	48.53
3	25.74	44.46
4	21.95	46.11
5	34.3814	44.6419
```

## Genotype file

Text file and VCF files are allowed. Overall,

- The pipeline requires bi-allelic SNP genotype data.
    - Example `bcftools` command: `bcftools view -m2 -M2 -v snps ${VCF}`
- The genotype data must not contain any missing values.
    - You can use tools like [beagle](https://faculty.washington.edu/browning/beagle/beagle.html) to impute missing data.
    - You can also filter the genotype data to retain only sites without missing data, e.g., `bcftools view -i 'N_MISSING == 0' ${VCF}`.
- It works best with diploid genotype data, but can handle any ploidy as long as it is consistent across all samples. Use caution when interpreting results for non-diploid data.
    - If heterozygous genotypes are not confidently called, you can force the data to be haploid (`option_geno$ploidy = 1`) by converting heterozygous genotypes to the alternative allele. Use caution when interpreting results.
- If the reference genome is divergent from the species/population of interest, set the major allele as the reference allele to avoid issues with ancestral state identification.

### Text genotype file

1. Files should be tab-delimited or comma-delimited.
2. Allowed file extensions are ".txt", ".txt.gz", ".csv", ".csv.gz", ".tsv", ".tsv.gz".
    1. For large files, it is recommended to compress the file with `gzip`.
3. No row or column names are allowed. Rows should be SNPs and columns should be samples.
4. Values in the file should be `0/1/.../ploidy`. In diploid organisms, the values can only be 0 or 1 or 2. Where values represent the number of alternative alleles for each genotype.
5. In the example below, the value of 1 at row 2 and column 3 represents that sample NO.3 has one copy of the alternative allele at SNP NO.2.

Example file with 5 samples and 10 SNPs:

```
0	0	2	0	0
0	0	1	0	0
1	2	0	1	2
0	0	2	0	0
0	0	1	0	0
1	2	0	1	2
0	0	2	0	0
0	0	1	0	0
1	2	0	1	2
0	0	2	0	0
```

### VCF genotype file

1. Any standard VCF files are allowed as long as they follow the specifications below.
2. VCF file handling is enabled by the `SeqArray` package.


### Notes on PLINK genotype file

1. PLINK genotype files are current not supported on the `marApp` platform but can be used with the `mar` package.
