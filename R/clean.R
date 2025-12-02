#' Cleans the ciTBI dataset
#' 
#' This function cleans the ciTBI dataset by addressing missing data,
#' changing variable names, and specifying variable types.
#' 
#' @param citbi The ciTBI dataframe
#' @return The cleaned ciTBI dataframe
#' @importFrom dplyr mutate rename
#' @export
clean <-
function(citbi) {
citbi <- citbi |>
 mutate(
 LocLen = ifelse(LocLen == 92, NA, LocLen),
 SeizLen = ifelse(SeizLen == 92, NA, SeizLen)
 )

citbi <- citbi |>
 mutate(
 Amnesia_verb = ifelse(Amnesia_verb == 91, NA, Amnesia_verb),
 HA_verb = ifelse(HA_verb == 91, NA, HA_verb)
 )

citbi <- citbi |>
 rename(patient_number = PatNum, amnesia = Amnesia_verb, loss_of_consciousness_length
= LocLen,
 seizure = Seiz, seizure_length = SeizLen, acting_normal = ActNorm,
 headache = HA_verb, vomiting = Vomit, dizziness = Dizzy,
 gcs_eye = GCSEye, gcs_verbal = GCSVerbal, gcs_motor = GCSMotor,
 gcs_total = GCSTotal, altered_mental_status = AMS, skull_fracture = SFxPalp,
 fontanelle_bulging = FontBulg, hematoma = Hema, clavicle_trauma = Clav,
 neurological_deficit = NeuroD, other_significant_injury = OSI, ct_form = CTForm1,
 age_in_month = AgeInMonth, gender = Gender, ct_done = CTDone,
 death_tbi = DeathTBI, citbi_outcome = PosIntFinal
 )
  
citbi <- citbi |>
 mutate(
 seizure = as.logical(seizure),
 acting_normal = as.logical(acting_normal),
 vomiting = as.logical(vomiting),
 dizziness = as.logical(dizziness),
 altered_mental_status = as.logical(altered_mental_status),
 skull_fracture = as.logical(skull_fracture),
 fontanelle_bulging = as.logical(fontanelle_bulging),
 hematoma = as.logical(hematoma),
 clavicle_trauma = as.logical(clavicle_trauma),
 neurological_deficit = as.logical(neurological_deficit),
 other_significant_injury = as.logical(other_significant_injury),
 ct_form = as.logical(ct_form),
 ct_done = as.logical(ct_done),
 death_tbi = as.logical(death_tbi),
 citbi_outcome = as.logical(citbi_outcome)
 )

citbi <- citbi |>
 mutate(
 loss_of_consciousness_length = factor(loss_of_consciousness_length,
 levels = c(1, 2, 3, 4),
 labels = c("<5 sec", "5 sec to <1 min", "1-5 min", ">5 min"))
 )

citbi <- citbi |>
 mutate(
 seizure_length = factor(seizure_length,
 levels = c(1, 2, 3, 4),
 labels = c("<1 min", "1-5 min", "5-15 min", ">15 min"))
 )

citbi <- citbi |>
 mutate(
 gender = factor(gender,
 levels = c(1, 2),
 labels = c("Male", "Female")
 )
)
}
