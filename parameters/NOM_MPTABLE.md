# NOM_MPTABLE.TBL Urban Land-Cover Modifications

## Purpose

`NOM_MPTABLE.TBL` is a modified Noah-MP parameter table for use with the
Noah-OWP-Modular (NOM) model in the NextGen modeling framework. The
modifications described here apply only to **USGS land-cover class 1
(Urban)**. Other land-cover classes are retained as reference classes
and are not intentionally modified.

The motivation is that the default Noah-MP USGS Urban class effectively
represents an urban grid cell as a nearly non-vegetated surface. That
assumption is not realistic at the spatial scales used by the National
Water Model and many other land-surface-model applications. A nominal
1-km2 grid cell classified as Urban commonly contains substantial lawns
and trees in addition to roofs, streets, parking areas, and small
amounts of bare soil.

A published study of urban LULC classification (Rahimi and Ebrahimian, 2024) 
https://www.sciencedirect.com/science/article/abs/pii/S0022169424012034
in a "typical" urban environment over a range of urban catchment sizes
found representative fractions of approximately:

-   impervious: 0.48
-   trees: 0.25
-   grass/lawns: 0.25
-   bare soil: 0.02

For purposes of the Noah-MP vegetation parameter estimation, the tree and
lawn fractions are combined into a **vegetated fraction of 0.50**. The
resulting conceptual partition is therefore:

    impervious = 0.48
    vegetated  = 0.50
    bare soil  = 0.02

Water fraction is not included in this representative partition because
measured urban water fractions vary substantially among locations.

The intent is not to replace Noah-MP's Urban class with a forest or
cropland class. The intent is to retain appropriate urban surface
characteristics while giving the vegetated portion of an Urban cell
physically reasonable canopy and vegetation properties.

## NextGen / NOM parameter data flow

The parameter values actually used by NOM are assembled from several
sources. Their order matters when interpreting or modifying
`NOM_MPTABLE.TBL`.

### 1. NextGen run-realization file

The NextGen realization selects the Noah-OWP-Modular BMI model and
identifies the catchment-specific NOM input file:

``` json
{
    "model_type_name": "NoahOWP",
    "init_config": "noah_input/{{id}}.input",
    "model_params": {
        "RSURF_EXP": 5.772704218684491,
        "CWP": 0.10241305784410278,
        "MP": 10.201001739788842,
        "VCMX25": 111.34518068017022,
        "MFSNO": 0.7009260409842241,
        "RSURF_SNOW": 34.137090364604866,
        "SCAMAX": 0.9335534803383965
    }
}
```

The `model_params` values are explicit parameter overrides.
Consequently, a value read from `NOM_MPTABLE.TBL` can subsequently be
replaced by a realization-specific value. This distinction is important
when deciding whether changing a table entry will affect a particular
NextGen simulation.

For example, the prototype realization explicitly overrides `CWP`, `MP`,
`VCMX25`, and `MFSNO`. Changes to corresponding default table values
therefore do not necessarily control the final values used in that run.

### 2. Catchment-specific NOM input file

During BMI initialization, NOM reads the catchment input file,
e.g. `cat-17536.input`. The relevant configuration includes:

``` text
&model_options
  dynamic_veg_option                = 1
  radiative_transfer_option         = 3
  sfc_drag_coeff_option             = 1
  canopy_stom_resist_option         = 1
  stomatal_resistance_option        = 4
  evap_srfc_resistance_option       = 4
/

&structure
  nveg             = 27
  vegtyp           = 1
  croptype         = 0
  sfctyp           = 1
/
```

`vegtyp = 1` selects the USGS Urban parameter entries.

`dynamic_veg_option = 1` is somewhat confusingly named. In this option,
prognostic dynamic vegetation is **off**, but Noah-MP uses the
prescribed monthly LAI and SAI values from the vegetation table. For
vegetation fraction, option 1 uses `SHDFAC`.

Thus, for this configuration:

-   LAI comes from the monthly `LAI_JAN` through `LAI_DEC` table values;
-   SAI comes from the monthly `SAI_JAN` through `SAI_DEC` table values;
-   `FVEG` is initialized from `SHDFAC`.

### 3. NOM_MPTABLE.TBL

The input file specifies the Noah-MP parameter-table filename. NOM reads
the selected table and extracts the entries corresponding to `vegtyp`.

For `dynamic_veg_option = 1`, the relevant NOM code path includes:

``` fortran
this%SHDFAC = SHDFAC_TABLE(namelist%vegtyp)
```

followed during interception/vegetation processing by:

``` fortran
parameters%FVEG = parameters%SHDFAC
```

subject to subsequent consistency checks.

The modular NOM source initializes `urban_flag` to `.false.`. Searches
of the current NOM Fortran source found no assignment that sets it
`.true.`. Therefore the special `urban_flag` branches appear inactive in
this NOM version unless some additional code outside the inspected
source changes the flag.

### 4. Run-realization parameter overrides

After the model is initialized, named `model_params` in the NextGen
realization can replace selected NOM parameter values. Therefore:

**MPTABLE values are defaults, not necessarily the final values used in
a calibrated NextGen realization.**

This is particularly important for interpreting sensitivity tests.

## Method used to construct Urban vegetation parameters

The vegetated portion of the representative Urban cell consists of
approximately equal areas of lawn/grass and trees. Existing USGS Noah-MP
classes were used as analogs:

-   **USGS class 3, Irrigated Cropland and Pasture**: surrogate for
    maintained urban grass/lawns;
-   **USGS class 15, Mixed Forest**: surrogate for urban tree cover.

For intrinsic vegetation parameters for which averaging is physically
appropriate, the Urban value is calculated over the **vegetated portion
only**:

\[ P\_{urban,veg} = 0.5 P\_{lawn} + 0.5 P\_{forest} \]

The 0.48 impervious fraction and 0.02 bare-soil fraction are **not**
included as zero-valued terms in intrinsic leaf or canopy-physiology
parameters. Doing so would incorrectly reduce the properties of the
vegetation itself and could effectively apply the vegetation fraction
twice.

Not every parameter is suitable for arithmetic averaging. Structural,
aerodynamic, categorical, inactive, and uniformly valued parameters are
treated separately as described below.

## Modified Urban parameters

### Monthly SAI

The default Urban SAI was zero for every month. This is inconsistent
with a cell containing substantial lawns and trees.

The Urban entries were replaced by representative values derived from
the lawn/tree/bare-soil urban mixture:

  Month     Urban SAI
  ------- -----------
  Jan            0.18
  Feb            0.18
  Mar            0.18
  Apr            0.18
  May            0.16
  Jun            0.18
  Jul            0.29
  Aug            0.34
  Sep            0.37
  Oct            0.35
  Nov            0.21
  Dec            0.18

These values also avoid the NOM effective-SAI cutoff under ordinary
snow-free conditions. NOM computes `ESAI = SAI * (1-FB)` and contains
logic that can set very small ESAI to zero.

### Monthly LAI

The default Urban LAI was zero for every month. The Urban entries were
replaced by area-weighted values representing lawns, trees, and the
small bare-soil component:

  Month     Urban LAI
  ------- -----------
  Jan            0.54
  Feb            0.54
  Mar            0.59
  Apr            0.70
  May            1.20
  Jun            1.66
  Jul            1.91
  Aug            1.75
  Sep            1.08
  Oct            0.59
  Nov            0.54
  Dec            0.54

With `dynamic_veg_option = 1`, these prescribed monthly values are the
intended LAI/SAI source.

### SHDFAC

Default Urban value:

``` text
SHDFAC = 0.10
```

Modified Urban value:

``` text
SHDFAC = 0.50
```

Reason: with `dynamic_veg_option = 1`, NOM sets `FVEG` from `SHDFAC`.
The observationally based representative Urban cell contains
approximately 25% trees plus 25% lawns, giving:

\[ FVEG = 0.25 + 0.25 = 0.50 \]

This value is also the quantity intended to be passed through the NOM
BMI as `FVEG` to CFE's `param_catchment_vegetated_fraction`.

For CFE, an independently specified impervious fraction of 0.48 then
permits calculation of:

\[ f\_{bare} = 1 - f\_{veg} - f\_{imp} = 1 - 0.50 - 0.48 = 0.02 \]

### NROOT

Default Urban value:

``` text
NROOT = 1
```

Modified Urban value:

``` text
NROOT = 4
```

The default single rooting layer is inconsistent with the substantial
mature-tree component of the revised Urban class. The revised value
allows the vegetation representation to include the deeper rooting
behavior associated with the tree component.

### RGL

`RGL` is the parameter used in the radiation-stress function.

Default Urban value:

``` text
RGL = 999.0
```

Lawn analog: 100.0\
Mixed-forest analog: 30.0

Modified Urban value:

``` text
RGL = 65.0
```

Calculation:

\[ RGL = 0.5(100) + 0.5(30) = 65 \]

The default 999 value is characteristic of the original effectively
non-vegetated Urban parameters set.

### RS

`RS` is leaf stomatal resistance in s/m.

Default Urban value:

``` text
RS = 200.0
```

Lawn analog: 40.0 s/m\
Mixed-forest analog: 125.0 s/m

Modified Urban value:

``` text
RS = 82.5
```

Calculation:

\[ RS = 0.5(40) + 0.5(125) = 82.5 {`\rm s/m`{=tex}} \]

The averaging is over the vegetated portion, not the entire grid-cell
area.

### HS

`HS` is the parameter used in the vapor-pressure-deficit stress
function.

Default Urban value:

``` text
HS = 999.0
```

Lawn analog: 36.25\
Mixed-forest analog: 51.93

Modified Urban value:

``` text
HS = 44.09
```

Calculation:

\[ HS = 0.5(36.25) + 0.5(51.93) = 44.09 \]

### Vegetation optical parameters

The default Urban class assigned zero leaf/stem optical properties,
consistent with its original non-vegetated treatment. Once Urban LAI and
SAI are nonzero, these zero values are internally inconsistent.

The following values use equal weighting of the lawn and mixed-forest
analogs within the vegetated portion.

  Parameter      Default Urban   Modified Urban
  ------------ --------------- ----------------
  `RHOL_VIS`              0.00             0.11
  `RHOL_NIR`              0.00             0.52
  `RHOS_VIS`              0.00             0.26
  `RHOS_NIR`              0.00             0.49
  `TAUL_VIS`              0.00             0.06
  `TAUL_NIR`              0.00             0.25
  `TAUS_VIS`              0.00             0.11
  `TAUS_NIR`              0.00             0.19

The stored values are rounded to a practical precision. For example, the
unrounded mixed values for `RHOL_VIS` and `RHOL_NIR` are 0.105 and
0.515.

### XL

`XL` describes leaf-angle distribution.

Lawn analog: -0.30\
Mixed-forest analog: 0.25

Modified Urban value:

``` text
XL = -0.025
```

Calculation:

\[ XL = 0.5(-0.30) + 0.5(0.25) = -0.025 \]

### CWPVT

`CWPVT` is the empirical canopy-wind parameter.

Default Urban value:

``` text
CWPVT = 0.18
```

Lawn analog: 1.67\
Mixed-forest analog: 0.29

A first-order mixed-canopy estimate is:

\[ CWPVT = 0.5(1.67) + 0.5(0.29) = 0.98 \]

Thus the revised table value is:

``` text
CWPVT = 0.98
```

This arithmetic average is an approximation because `CWPVT` participates
nonlinearly in the canopy-wind formulation. It is nevertheless a more
representative default for the synthetic lawn/tree canopy than the
original Urban value.

**Important:** in the prototype NextGen realization, `CWP` is explicitly
supplied in `model_params`. That realization-specific override can
supersede the table-derived/default canopy-wind parameter. The table
change therefore defines a better default but may not control a
calibrated NextGen run.

## Urban parameters intentionally left unchanged

### Z0MVT

`Z0MVT` is momentum roughness length.

Default Urban:

``` text
Z0MVT = 1.00 m
```

This was retained. Momentum roughness is an aerodynamic property of the
complete urban surface, including buildings and other structures. It is
not an intrinsic vegetation property, and simply averaging lawn and
forest roughness would discard a physically important urban
characteristic.

### HVT

`HVT` is top-of-canopy height.

Default Urban:

``` text
HVT = 15.0 m
```

This was retained. A 15-m canopy top is already consistent with mature
trees in many urban and suburban regions and is close to the Mixed
Forest value. A lawn/tree arithmetic average would not be an appropriate
definition of canopy-top height.

### HVB

`HVB` is bottom-of-canopy height. This parameter was considered
separately because it describes canopy geometry rather than an intrinsic
leaf property. Where the Urban canopy is interpreted as a mature-tree
overstory, a forest-like value can be justified. Any change to `HVB`
should be documented as a structural canopy assumption rather than as an
area-weighted vegetation parameter.

### DEN

`DEN` is tree density (trunks/m2).

Inspection of the Noah-MP source found its relevant physics expression
commented out in the code version examined. Because it is not active in
the current configuration/code path, it was not tuned.

### RC

`RC` is tree crown radius.

For the examined configuration:

``` text
radiative_transfer_option = 3
```

the crown-radius-dependent radiation branch is not used. `RC` was
therefore left unchanged rather than modifying an inactive parameter.

### MFSNO

`MFSNO` is a snow-cover/melt parameter. The active default table assigns
the same value across the USGS classes, so there is no Urban-specific
table defect to correct.

In addition, the prototype NextGen realization explicitly supplies an
`MFSNO` value through `model_params`, so that run does not rely solely
on the table default.

### SCFFAC

`SCFFAC` was not found in the examined Noah-MP LSM source code. It was
therefore left unchanged.

### C3PSN

`C3PSN` selects the photosynthetic pathway (`0 = C4`, `1 = C3`). This is
categorical rather than a parameter suitable for arithmetic averaging.
The Urban lawn/tree mixture is represented as C3, so the existing value
is retained.

### KC25, AKC, KO25, AKO, AVCMX, AQE

These parameters have the same values across the relevant vegetation
classes. There is no Urban-specific discrepancy to correct, so they are
retained.

### Dynamic vegetation / carbon parameters

The following parameters were examined but intentionally not changed:

``` text
LTOVRC
DILEFC
DILEFW
RMF25
SLA
FRAGR
TMIN
VCMX25
TDLEF
BP
MP
QE25
RMS25
RMR25
ARM
FOLNMX
WDPOOL
WRRAT
MRP
```

With `dynamic_veg_option = 1`, the prognostic dynamic-vegetation/carbon
pathway is disabled. Monthly LAI/SAI are prescribed instead. Parameters
used only by the inactive dynamic-vegetation/carbon machinery therefore
should not be changed merely to make the Urban table look numerically
similar to vegetated classes.

Several of these names also appear as NextGen realization
`model_params`. In particular, the prototype realization explicitly
overrides `MP` and `VCMX25`. This is another reason to distinguish table
defaults from runtime calibrated parameters.

## FVEG and the NOM-CFE coupling

The revised Urban representation is designed to work with the CFE3.1
land-cover partition.

NOM provides:

``` text
FVEG = 0.50
```

from the revised Urban `SHDFAC` when `dynamic_veg_option = 1`.

The NOM-CFE BMI coupling driver passes this value to CFE using:

``` c
model->set_value(model,
                 "param_catchment_vegetated_fraction",
                 &nom_fveg);
```

CFE independently receives or reads:

``` text
impervious_fraction = 0.48
```

and calculates:

``` text
bare_soil_fraction = 1.0 - vegetated_fraction - impervious_fraction
                   = 1.0 - 0.50 - 0.48
                   = 0.02
```

This keeps the roles of the two models clean:

-   NOM supplies the time-step vegetation fraction through its BMI
    output;
-   CFE receives that vegetation fraction through its BMI input;
-   CFE retains its independent impervious-fraction parameter;
-   CFE diagnoses the residual bare-soil fraction.

The BMI variable name is `param_catchment_vegetated_fraction`; older
driver code using `param_catchment_forested_fraction` is stale and will
cause `set_value()` to fail.

## Important distinction: Urban land-cover class versus impervious fraction

The fundamental reason for these modifications is that a categorical
Urban LULC classification must not be interpreted as equivalent to 100%
impervious area at kilometer and larger spatial scales.

The revised parameter set treats USGS Urban as a heterogeneous
land-cover class with an explicit subgrid partition. It therefore
preserves the categorical Urban classification while permitting Noah-MP
and CFE to represent the vegetation, impervious area, and bare soil that
actually coexist within an Urban grid cell.

## Validation priorities

The revised table should be validated by checking, at minimum:

1.  NOM reports `FVEG = 0.50` for USGS Urban catchments using
    `dynamic_veg_option = 1`.
2.  Monthly NOM LAI and SAI follow the revised Urban table values.
3.  NOM produces nonzero and physically plausible PET/ET for Urban
    catchments.
4.  The NOM-CFE driver successfully passes `FVEG` to CFE as
    `param_catchment_vegetated_fraction`.
5.  CFE reports approximately 0.50 vegetated, 0.48 impervious, and 0.02
    bare-soil fractions for the representative Urban case.
6.  Realization-level `model_params` are recorded when interpreting
    results, because calibrated overrides can supersede table defaults.
7.  Urban energy, snow, runoff, and aerodynamic behavior are checked
    separately before altering additional Urban-specific Noah-MP logic.

## Scope

These changes are intended as a physically motivated revision of the
**default USGS Urban parameters** used by NOM/Noah-MP in this
NextGen configuration. They are not intended to imply that all urban
areas have identical land-cover fractions. The 0.48/0.50/0.02 partition
is a representative default derived from observed urban land-cover
fractions and should eventually be tested across multiple urban
catchments and climatic regions.
