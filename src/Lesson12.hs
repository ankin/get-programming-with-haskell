module Lesson12 where

data RhType = Pos | Neg

data ABOType = A | B | AB | O

data BloodType = BloodType ABOType RhType

patientA :: BloodType
patientA = BloodType A Pos

showRh Pos = "+"
showRh Neg = "-"

showAbo :: ABOType -> [Char]
showAbo A = "A"
showAbo B = "B"
showAbo AB = "AB"
showAbo O = "O"

showBloodType :: BloodType -> [Char]
showBloodType (BloodType abo rh) = showAbo abo ++ showRh rh

canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _ = True
canDonateTo _ (BloodType AB _) = True
canDonateTo (BloodType A _) (BloodType A _) = True
canDonateTo (BloodType B _) (BloodType B _) = True
canDonateTo _ _ = False

type Female = Char
type Male = Char
data Sex = Male | Female
type Age = Int
type Height = Int
type FirstName = String
type MiddleName = String
type LastName = String

data Name = Name FirstName LastName | MiddleName FirstName MiddleName LastName

showName (Name f l) = f ++ " " ++ l
showName (MiddleName f m l) = f ++ " " ++ m ++ " " ++ l

-- data Patient = Patient Name Age Sex Height BloodType

data Patient = Patient {name :: Name, age :: Age, sex :: Sex, height :: Height, bloodType :: BloodType}

showPatient patient = showName (name patient) ++ " " ++ showBloodType (bloodType patient)

-- janeESmith = Patient (MiddleName "Jane" "Elizabeth" "Smith") 30 Female 165 (BloodType O Neg)

-- janeESmith = Patient {name = MiddleName "Jane" "Elizabeth" "Smith", age = 30, sex = Female, height = 165, bloodType = BloodType O Neg}

patientCanDonateTo Patient {bloodType = bloodTypeA} Patient {bloodType = bloodTypeB} = canDonateTo bloodTypeA bloodTypeB



