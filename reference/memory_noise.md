# Memory bias under relative-noise manipulations

A compact participant-level dataset derived from Experiment 1 of
Chetverikov and Hansmann-Roth (2026). The original Experiment 1
contained two independent samples. In Exp. 1 the high-noise stimulus had
SD = 20 degrees; in Exp. 1 HV it had SD = 45 degrees. Low-noise stimuli
had SD = 5 degrees in both samples.

## Usage

``` r
data(memory_noise)
```

## Format

A data.frame with 70 observations and 6 variables.

## Source

Derived from the public behavioral dataset at <https://osf.io/kqb8t/>.
The package subset is based on the canonical CHR2026 representation used
for model comparison.

## Details

Only unequal-noise trials are included. Bias is estimated with the same
weighted probability-density asymmetry approach used in the paper and
averaged over target/non-target dissimilarities from 1 to 44 degrees,
the range in which Experiment 1 showed the relative-noise interaction.
Positive values indicate attraction toward the competing non-target and
negative values indicate repulsion.

- participant. An anonymized participant identifier, unique across the
  two samples.

- experiment. Between-subject sample: `"Exp. 1"` or `"Exp. 1 HV"`.

- high_noise_sd. High stimulus-noise standard deviation used in the
  sample (20 or 45 degrees).

- relative_noise. Ordered within-subject factor: `"target less noisy"`
  or `"target more noisy"`.

- bias_percent. Density-asymmetry bias in percentage points. Positive
  values indicate attraction toward the non-target; negative values
  indicate repulsion.

- n_trials. Number of non-outlier unequal-noise trials contributing to
  the weighted bias curve.

## References

Chetverikov, A., & Hansmann-Roth, S. (2026). Noise in Competing
Representations Determines the Direction of Memory Biases. eLife, 15,
RP111380.
[doi:10.7554/eLife.111380.1](https://doi.org/10.7554/eLife.111380.1)
