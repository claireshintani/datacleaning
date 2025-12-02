\name{clean}
\alias{clean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A Capitalized Title (ideally limited to 65 characters)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
clean(citbi)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{citbi}{
%%     ~~Describe \code{citbi} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or standard data sets, see data().

## The function is currently defined as
function (citbi) 
{
    citbi <- mutate(citbi, LocLen = ifelse(LocLen == 92, NA, 
        LocLen), SeizLen = ifelse(SeizLen == 92, NA, SeizLen))
    citbi <- mutate(citbi, Amnesia_verb = ifelse(Amnesia_verb == 
        91, NA, Amnesia_verb), HA_verb = ifelse(HA_verb == 91, 
        NA, HA_verb))
    citbi <- rename(citbi, patient_number = PatNum, amnesia = Amnesia_verb, 
        loss_of_consciousness_length = LocLen, seizure = Seiz, 
        seizure_length = SeizLen, acting_normal = ActNorm, headache = HA_verb, 
        vomiting = Vomit, dizziness = Dizzy, gcs_eye = GCSEye, 
        gcs_verbal = GCSVerbal, gcs_motor = GCSMotor, gcs_total = GCSTotal, 
        altered_mental_status = AMS, skull_fracture = SFxPalp, 
        fontanelle_bulging = FontBulg, hematoma = Hema, clavicle_trauma = Clav, 
        neurological_deficit = NeuroD, other_significant_injury = OSI, 
        ct_form = CTForm1, age_in_month = AgeInMonth, gender = Gender, 
        ct_done = CTDone, death_tbi = DeathTBI, citbi_outcome = PosIntFinal)
    citbi <- mutate(citbi, seizure = as.logical(seizure), acting_normal = as.logical(acting_normal), 
        vomiting = as.logical(vomiting), dizziness = as.logical(dizziness), 
        altered_mental_status = as.logical(altered_mental_status), 
        skull_fracture = as.logical(skull_fracture), fontanelle_bulging = as.logical(fontanelle_bulging), 
        hematoma = as.logical(hematoma), clavicle_trauma = as.logical(clavicle_trauma), 
        neurological_deficit = as.logical(neurological_deficit), 
        other_significant_injury = as.logical(other_significant_injury), 
        ct_form = as.logical(ct_form), ct_done = as.logical(ct_done), 
        death_tbi = as.logical(death_tbi), citbi_outcome = as.logical(citbi_outcome))
    citbi <- mutate(citbi, loss_of_consciousness_length = factor(loss_of_consciousness_length, 
        levels = c(1, 2, 3, 4), labels = c("<5 sec", "5 sec to <1 min", 
            "1-5 min", ">5 min")))
    citbi <- mutate(citbi, seizure_length = factor(seizure_length, 
        levels = c(1, 2, 3, 4), labels = c("<1 min", "1-5 min", 
            "5-15 min", ">15 min")))
    citbi <- mutate(citbi, gender = factor(gender, levels = c(1, 
        2), labels = c("Male", "Female")))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
