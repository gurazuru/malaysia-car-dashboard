# This script categorizes the model list into defined segments

library(dplyr)
library(stringr)

unique_models <- model_list_top # fetch data from setup.R

list_mini       <- c("Axia", "Kelisa", "Kenari", "Viva", "Alto")

list_hatchback  <- c("Myvi", "Jazz", "Iriz", "Yaris", "Dolphin", "Fiesta", "I30",
                     "MG4", "Mirage", "Picanto", "V", "Leaf", "Swift", "Golf", "Cooper",
                     "Ora", "e.MAS 5", "Satria", "Satria Neo", "Clio", "Twizy", "Zoe", "i10",
                     "Suprima S", "308", "Beetle", "208", "CR-Z", "Focus", "Atos", "207", "B-Class",
                     "Gen2", "Savvy", "N-Box", "Clubman", "John Cooper Works", "Scirocco", "Brabus",
                     "Veloster", "Getz", "New Beetle")

list_sedana     <- c("Bezza", "Saga", "Persona", "Wira")

list_sedanb     <- c("City", "Vios", "Almera", "Corolla Altis", "Accent", "Rio", 
                     "Attrage", "Polo", "2", "Seal 6", "Cruze", "Elantra", "Ioniq",
                     "3", "MG5", "Lancer", "Cerato", "Forte", "Latio", "Rush", "Inspira",
                     "Sylphy", "Vento", "Sonic")

list_sedancd    <- c("S70", "Civic", "Preve", "Camry", "6", "Accord", "Ioniq 6", "Teana", 
                     "Optima", "Passat", "Prius", "Seal", "Model 3", "Sonata", "Perdana", 
                     "Insight", "Waja", "Jetta", "408", "508", "Sentra", "86", "Mark X",
                     "GR86", "A3", "B14", "407", "RCZ", "Fluence", "BRZ", "Impreza", "S40")

list_suvb       <- c("WR-V", "HR-V", "Ativa", "X50", "CX-3", "CX-5", "CX-30", "Corolla Cross", 
                     "XV", "Omoda 5", "Tiggo", "ASX", "Forester", "Atto 3", "Kicks", "iCaur 03",
                     "iCaur V23", "B10", "Traz", "Atto 2", "C-HR", "Captur", "2008", "SX4", "Jimny",
                     "Haval H1", "Haval M4", "GS3", "Jimny Sierra", "Ecosport", "Haval H2", "Kona",
                     "Jaecoo J5", "QV-E", "S5", "Xforce")

list_suvcd      <- c("CR-V", "X70", "X-Trail", "Harrier", "Jaecoo J7", "Jaecoo J8", "X90", 
                     "Fortuner", "CX-8", "CX-9", "e.MAS 7", "3008", "Outlander", "Model Y", "Omoda 9", 
                     "Sealion", "Captiva", "Tucson", "7X", "C10", "Sportage", "Pajero", "Santa Fe",
                     "Haval H6", "Sorento", "Dashing", "FJ Cruiser", "Kuga", "RAV4", "Grand Vitara",
                     "Murano", "G6", "Tank 300", "Escape", "Everest", "MU-X", "Wrangler", "VT9",
                     "CX-60", "CX-7", "Koleos", "MG HS", "ZS", "ZS EV")

list_mpv        <- c("Alza", "Aruz", "Xpander", "Veloz", "Exora", "Alphard",
                     "Vellfire", "Livina", "Grand Livina", "Avanza", "Innova", "Estima", "BR-V",
                     "Serena", "Wish", "Grand Starex", "5008", "Citra", "M6", "Ertiga", "Odyssey",
                     "Innova Zenix", "9", "Carnival", "Stream", "Voxy", "Eastar", "Sienta", "STEPWGN",
                     "Freed", "Staria", "Biante", "G10", "Grandis", "Citra II Rondo", "Noah", "8",
                     "D9", "Touran", "5", "Stavic", "009", "Orlando", "Elysion", "Matrix", "Elgrand",
                     "Previa", "Sharan", "X9")

list_pickup     <- c("Hilux", "Triton", "D-Max", "Ranger", "Navara", "BT-50", "Invader", "Colorado", 
                     "Frontier", "Vigus", "Wingle")

list_sedanlux   <- c("A-Class", "C-Class", "E-Class", "S-Class", "1 Series", "2 Series", "4 Series",
                     "3 Series", "5 Series", "6-Series", "6 Series", "7 Series", "iX", "iX1",
                     "A4", "A5", "A6", "TT", "CT", "ES", "IS", "LX",
                     "S60", "V40", "Mustang", "Arteon", "SLK", "CC", "CLS", "GT-R", "MX-5",
                     "718 Cayman", "911 Carrera", "Cayenne", "Panamera", "Taycan", "Z4", "i4",
                     "i5", "i7", "Continental Flying Spur", "Continental GT", "XF", "XJ", "GS", "EQE", "EQS", "718 Boxster",
                     "Crown", "Supra", "S90", "Cyberster")

list_suvlux     <- c("RX", "NX", "GLA", "GLB", "GLC", "GLE", "CLA", "Tiguan", "X5", "X3", "X1", 
                     "Land Cruiser", "Countryman", "Q3", "Q5", "Q7", "X4", "X6", "Macan",
                     "XC40", "XC60", "XC90", "Range Rover Evoque", "Defender", "Land Cruiser Prado",
                     "Range Rover Sport", "Range Rover", "G-Class", "Range Rover Velar",
                     "X7", "iX2", "Yukon XL", "Urus", "Discovery", "Range Rover Vogue", "LBX", "UX",
                     "M-Class", "Touareg", "C40")

list_van        <- c("Hiace", "Urvan", "Vanette", "View", "Placer-X", "Era", "V80", "Pregio",
                     "Xiamen Placer", "Kingo", "Caldina", "Placer", "Era Commuter")


master_ref <- unique_models |>
  mutate(segment = case_when(
     
      model %in% list_mini ~ "Mini",
      model %in% list_hatchback ~ "Hatchback",
      model %in% list_sedana ~ "Sedan-A",
      model %in% list_sedanb ~ "Sedan-B",
      model %in% list_sedancd ~ "Sedan-C/D",
      model %in% list_suvb ~ "SUV-B",
      model %in% list_suvcd ~ "SUV-C/D",
      model %in% list_mpv ~ "MPV",
      model %in% list_pickup ~ "Pick-Up",
      model %in% list_sedanlux ~ "Sedan-Luxury",
      model %in% list_suvlux ~ "SUV-Luxury",
      model %in% list_van ~ "Van",
      
      TRUE ~ "Unspecified"
    )  
  ) |>
  mutate(segment = case_when(
   
      # Explicit disambiguation steps
      maker == "Polestar" & model == "2" ~ "Sedan-Luxury",
      maker == "Seres" & model == "3" ~ "SUV-B",
      maker == "MG" & model == "5" ~ "Sedan-B",
      
      TRUE ~ segment
    )  
  )

master_ref_path <- "Data/master_ref.csv"
write_csv(master_ref, master_ref_path)

