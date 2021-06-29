Passed by reference vars in NOAH-MP
================
Keith Jennings
4/21/2021
​
## Subroutine calls and arguments
​
In Fortran, variables are typically passed by reference, meaning the
arguments for a subroutine point to a location in memory corresponding
to the variables in the call. In this case, the variable in position 1
of a call corresponds to the variable in position 1 of the subroutine
arguments and so on and so forth. Variables need to match types, BUT the
name can be changed between the call and the argument. In this case, we
are still pointing to the same memory location, but we are doing so with
a different variable name in the subroutine.
​
For example, we could see the following call:
​
`CALL COMPUTATIONX(XX,YY)`
​
with the corresponding subroutine definition:
​
`SUBROUTINE COMPUTATION(X1,Y1)`
​
In this case, `X1` and `Y1` would be used throughout the subroutine,
while referring to `XX` and `YY`, respectively.
​
## Why this matters in Modular NOAH-MP
​
There are several instances in NOAH-MP where variables passed by
reference change name. Normally this isn’t a problem, but it is now in
conflict with our use of types. (We could still append the variables
changing name to the call and subroutine arguments, but this would
partially defeat the use of types.)
​
We now need to make sure that variables within types use consistent
nomenclature, i.e. we don’t want to be changing names across
subroutines.
​
## Detecting variables passed by reference with changed names
​
To enumerate which variables change name when passed by reference, I
compiled all energy-related calls and subroutine arguments. I pasted all
the variables into a single CSV file, which is imported below:
​
``` r
vars <- read.csv("noah_mp_passed_by_reference.csv") %>% 
  mutate(across(where(is.character), str_trim)) %>% 
  mutate(across(where(is.character), tolower)) # set all to lowercase, Fortran is case insensitive
```
​
The data frame `vars` includes the subroutine name, the call, the
arguments, and which number call for which the comparison is made.
​
We can now filter to calls and arguments with conflicting names:
​
``` r
vars_conflict <- vars %>% 
  filter(call != argument)
```
​
These are shown below:
​
``` r
knitr::kable(vars_conflict)
```
​
| subroutine   | call                 | argument      | call\_num |
| :----------- | :------------------- | :------------ | --------: |
| noahmp\_sflx | i                    | iloc          |         1 |
| noahmp\_sflx | j                    | jloc          |         1 |
| noahmp\_sflx | dz8w1d               | dz8w          |         1 |
| noahmp\_sflx | fveg                 | shdfac        |         1 |
| noahmp\_sflx | fvgmax               | shdmax        |         1 |
| noahmp\_sflx | t\_ml                | sfctmp        |         1 |
| noahmp\_sflx | p\_ml                | sfcprs        |         1 |
| noahmp\_sflx | u\_ml                | uu            |         1 |
| noahmp\_sflx | v\_ml                | vv            |         1 |
| noahmp\_sflx | q\_ml                | q2            |         1 |
| noahmp\_sflx | swdn                 | soldn         |         1 |
| noahmp\_sflx | co2pp                | co2air        |         1 |
| noahmp\_sflx | o2pp                 | o2air         |         1 |
| noahmp\_sflx | z\_ml                | zlvl          |         1 |
| noahmp\_sflx | smh2o                | sh2o          |         1 |
| noahmp\_sflx | qsfc1d               | qsfc          |         1 |
| noahmp\_sflx | sndpth               | snowh         |         1 |
| noahmp\_sflx | swe                  | sneqv         |         1 |
| noahmp\_sflx | plai                 | lai           |         1 |
| noahmp\_sflx | psai                 | sai           |         1 |
| noahmp\_sflx | esoil                | edir          |         1 |
| noahmp\_sflx | q2mv                 | q2v           |         1 |
| noahmp\_sflx | q2mb                 | q2b           |         1 |
| noahmp\_sflx | runsf                | runsrf        |         1 |
| noahmp\_sflx | runsb                | runsub        |         1 |
| noahmp\_sflx | fvegmp               | fveg          |         1 |
| noahmp\_sflx | salb                 | albedo        |         1 |
| energy       | zlvl                 | zref          |         1 |
| vege\_flux   | tgv                  | tg            |         1 |
| vege\_flux   | cmv                  | cm            |         1 |
| vege\_flux   | chv                  | ch            |         1 |
| vege\_flux   | ghv                  | gh            |         1 |
| vege\_flux   | chv2                 | cah2          |         1 |
| bare\_flux   | zpdg                 | zpd           |         1 |
| bare\_flux   | z0mg                 | z0m           |         1 |
| bare\_flux   | latheag              | lathea        |         1 |
| bare\_flux   | gammag               | gamma         |         1 |
| bare\_flux   | cmb                  | cm            |         1 |
| bare\_flux   | chb                  | ch            |         1 |
| bare\_flux   | vegtyp               | ivgtyp        |         1 |
| bare\_flux   | chb2                 | ehb2          |         1 |
| tdfcnd       | iz                   | isoil         |         1 |
| tdfcnd       | df(iz)               | df            |         1 |
| tdfcnd       | smc(iz)              | smc           |         1 |
| tdfcnd       | sh2o(iz)             | sh2o          |         1 |
| twostream    | tv                   | t             |         1 |
| twostream    | fabd                 | fab           |         1 |
| twostream    | albd                 | fre           |         1 |
| twostream    | ftdd                 | ftd           |         1 |
| twostream    | ftid                 | fti           |         1 |
| twostream    | frevd                | frev          |         1 |
| twostream    | fregd                | freg          |         1 |
| twostream    | tv                   | t             |         2 |
| twostream    | fabi                 | fab           |         2 |
| twostream    | albi                 | fre           |         2 |
| twostream    | ftdi                 | ftd           |         2 |
| twostream    | ftii                 | fti           |         2 |
| twostream    | frevi                | frev          |         2 |
| twostream    | fregi                | freg          |         2 |
| esat         | esatw                | esw           |         1 |
| esat         | esati                | esi           |         1 |
| esat         | dsatw                | desw          |         1 |
| esat         | dsati                | desi          |         1 |
| sfcdif2      | z0m                  | z0            |         1 |
| sfcdif2      | tah                  | thz0          |         1 |
| sfcdif2      | thair                | thlm          |         1 |
| sfcdif2      | ur                   | sfcspd        |         1 |
| sfcdif2      | zlvl                 | zlm           |         1 |
| sfcdif2      | cm                   | akms          |         1 |
| sfcdif2      | ch                   | akhs          |         1 |
| sfcdif2      | moz                  | rlmo          |         1 |
| sfcdif2      | wstar                | wstar2        |         1 |
| sfcdif2      | fv                   | ustar         |         1 |
| ragrb        | vaie                 | vai           |         1 |
| stomata      | parsun               | apar          |         1 |
| stomata      | estv                 | ei            |         1 |
| stomata      | eah                  | ea            |         1 |
| stomata      | o2air                | o2            |         1 |
| stomata      | co2air               | co2           |         1 |
| stomata      | rssun                | rs            |         1 |
| stomata      | psnsun               | psn           |         1 |
| stomata      | parsha               | apar          |         2 |
| stomata      | estv                 | ei            |         2 |
| stomata      | eah                  | ea            |         2 |
| stomata      | o2air                | o2            |         2 |
| stomata      | co2air               | co2           |         2 |
| stomata      | rssha                | rs            |         2 |
| stomata      | psnsha               | psn           |         2 |
| canres       | parsun               | par           |         1 |
| canres       | tv                   | sfctmp        |         1 |
| canres       | btran                | rcsoil        |         1 |
| canres       | rssun                | rc            |         1 |
| canres       | psnsun               | psn           |         1 |
| canres       | parsha               | par           |         2 |
| canres       | tv                   | sfctmp        |         2 |
| canres       | btran                | rcsoil        |         2 |
| canres       | rssha                | rc            |         2 |
| canres       | psnsha               | psn           |         2 |
| gecros       | julian               | doy           |         1 |
| gecros       | 1                    | crop          |         1 |
| gecros       | rahc                 | rt            |         1 |
| gecros       | rahg+rsurf           | rts           |         1 |
| gecros       | ur                   | wn            |         1 |
| gecros       | swdown               | rsd           |         1 |
| gecros       | lwdn                 | rld           |         1 |
| gecros       | parameters%smcwlt(1) | wcmin         |         1 |
| gecros       | parameters%dleaf     | lwidth        |         1 |
| gecros       | gecros1d             | state\_gecros |         1 |
| gecros       | sav                  | atrjc         |         1 |
| gecros       | sag                  | atrjs         |         1 |
| gecros       | rssun                | arswsu        |         1 |
| gecros       | rssha                | arswsh        |         1 |
| sfcdif2      | z0m                  | z0            |         2 |
| sfcdif2      | tgb                  | thz0          |         2 |
| sfcdif2      | thair                | thlm          |         2 |
| sfcdif2      | ur                   | sfcspd        |         2 |
| sfcdif2      | zlvl                 | zlm           |         2 |
| sfcdif2      | cm                   | akms          |         2 |
| sfcdif2      | ch                   | akhs          |         2 |
| sfcdif2      | moz                  | rlmo          |         2 |
| sfcdif2      | wstar                | wstar2        |         2 |
| sfcdif2      | fv                   | ustar         |         2 |
| hrt          | zbotsno              | zbot          |         1 |
| hrt          | eflxb                | botflx        |         1 |
| rosr12       | ci                   | p             |         1 |
| rosr12       | ai                   | a             |         1 |
| rosr12       | bi                   | b             |         1 |
| rosr12       | ciin                 | c             |         1 |
| rosr12       | rhstsin              | d             |         1 |
| rosr12       | rhsts                | delta         |         1 |
| rosr12       | isnow+1              | ntop          |         1 |
| frh2o        | j                    | isoil         |         1 |
| frh2o        | supercool(j)         | free          |         1 |
| frh2o        | stc(j)               | tkelv         |         1 |
| frh2o        | smc(j)               | smc           |         1 |
| frh2o        | sh2o(j)              | sh2o          |         1 |
​
We can now use this table when updating the data types, calls, and
subroutine arguments.
