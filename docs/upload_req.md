When uploading your custom files for MAR calculations,
please ensure that the files follow the formatting guidelines below:

**Notes on sensitive data**

The marApp portal stores and processes all data on [shinyapps.io](https://www.shinyapps.io) servers.
Although shinyapps.io is secure-by-design, users should avoid uploading sensitive data (e.g. endangered species locations) without appropriate permissions and safeguards.

**Coordinate file**

1. Files should be tab-delimited or comma-delimited.
2. Allowed file extensions are ".txt", ".txt.gz", ".csv", ".csv.gz", ".tsv", ".tsv.gz".
3. The first line should consist of exactly three columns: `ID`,`LON`,`LAT` (or `ID`,`LONGITUDE`,`LATITUDE`), in that order. No extra columns are allowed.
    1. If VCF genotype file is provided, the `ID` column should match the sample IDs in the VCF file.
    2. If text genotype file is provided, the `ID` column should be sequential integers starting from 1. Please make sure that the samples are in the same order as the genotype file.
4. Sample IDs must be unique, and every sample with genotype data must have a paired longitude and latitude.
5. Coordinates are interpreted as decimal degrees on WGS84 (EPSG:4326).
    - If coordinates span more than 10 degrees, coordinates are reprojected onto the Equal Earth Greenwich (EPSG: 8857) to preserve equal area per grid cell. Otherwise, coordinates are kept on WGS84 (EPSG:4326) for downstream analysis.
6. No missing values allowed.

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

Text file and VCF files are allowed. Overall,

- The pipeline requires bi-allelic SNP sites.
    - Example `bcftools` command: `bcftools view -m2 -M2 -v snps ${VCF}`
- Missing data is allowed and accounted for. `mar` applies `pixy`'s site-level denominator and effective sequence length adjustments, which keep `thetaw` and `thetapi` unbiased in the presence of missing genotypes. Excessive missingness will still bias the summary, so we recommend imputing beforehand if that's the case.
    - You can use tools like [beagle](https://faculty.washington.edu/browning/beagle/beagle.html) to impute missing data.
    - You can also filter the genotype data to retain only sites without missing data, e.g., `bcftools view -i 'N_MISSING == 0' ${VCF}`.
- The `marApp` only supports diploid organisms. For handling of other ploidy, please use the `mar` R package.
- If the reference genome is divergent from the species/population of interest, set the major allele as the reference allele to avoid issues with ancestral state identification.

*Text genotype file*

1. Files should be tab-delimited or comma-delimited.
2. Allowed file extensions are ".txt", ".txt.gz", ".csv", ".csv.gz", ".tsv", ".tsv.gz".
    1. For large files, it is recommended to compress the file with `gzip`.
3. No row or column names are allowed. Rows should be SNPs and columns should be samples.
4. Values in the file should be `0/1/.../ploidy`. In diploid organisms, the values can only be 0 or 1 or 2. Where values represent the number of alternative alleles for each genotype.
5. Missing genotypes should be marked as `NA`.
6. In the example below, the value of 1 at row 2 and column 3 represents that sample NO.3 has one copy of the alternative allele at SNP NO.2.

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

*VCF genotype file*

1. Any standard VCF files are allowed as long as they follow the specifications below.
2. Allowed file extensions are ".vcf" and ".vcf.gz".
3. Only the `GT` field is read. Each call is converted to a count of alternative alleles, and missing calls (`./.`) become `NA`. Sample IDs, chromosomes and positions are taken from the VCF itself.
4. The whole file is read into memory, so please upload a modest SNP panel rather than a whole-genome all-sites VCF.

