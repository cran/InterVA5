# InterVA5 - changes

Version 1.1.0 (2020-04-24) 
==========================
* Updating symptom-cause-information matrix (probbaseV5) to match the InterVA5_1
  release (April 3, 2020) on interva.net.


Version 1.0.4 (2019-08-08) 
==========================
* Add function to download the symptom-cause-information (SCI) matrix, probbaseV5, from:
  http://www.byass.uk/interva/InterVA_5_v5.0_release.zip  
  The updated SCI can be passed to InterVA5 and used to assign causes of death.  An older
  version of probbase (v14) and the current version are also stored as data files
  (probbaseV5_14 and probbaseV5_17).
* Added new functions to return the symptoms with the highest conditional probability
  of observing the symptom, given the death is due to a particular cause.
* Updated the InterVA5() function to return the checked data (if requested).

Version 1.0.3 (2018-11-20) 
==========================
* Fix output csv file format error
* Fix an extra space in cause-of-death name string 'Other and unspecified NCD'

Version 1.0.2 (2018-07-16)
==========================
* Add InSilicoVA rule of data check.
* Fix typo in checking WHO 2016 input using InterVA5 rules.
