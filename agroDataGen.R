agroDataGen <- function(num_fields, num_crops, total_area, total_production) {
  # Λίστα διαθέσιμων καλλιεργειών
  crops <- c("Wheat", "Corn", "Barley", "Sunflower", "Cotton",
             "Tomato", "Potato", "Rice")
  
  # Τυχαία επιλογή των καλλιεργειών που θα χρησιμοποιηθούν
  selected_crops <- sample(crops, num_crops)
  
  # Δημιουργία κωδικών για αγροτεμάχια (F1, F2, ...)
  fields <- paste0("F", 1:num_fields)
  
  # Δημιουργία όλων των συνδυασμών αγροτεμαχίου-καλλιέργειας
  combinations <- expand.grid(Field = fields, Crop = selected_crops, stringsAsFactors = FALSE)
  
  # Πλήθος γραμμών του πίνακα (αγροτεμάχιο x καλλιέργεια)
  n <- nrow(combinations)
  
  # Τυχαίες αρχικές τιμές (ομοιόμορφα κατανεμημένες)
  raw_area <- runif(n)
  raw_production <- runif(n)
  
  # Κανονικοποίηση για να τηρηθούν τα όρια του χρήστη
  norm_area <- round(raw_area / sum(raw_area) * total_area, 2)
  norm_production <- round(raw_production / sum(raw_production) * total_production, 2)
  
  # Διόρθωση τελευταίας τιμής ώστε να είναθ ίση με τα συνολικά όρια
  norm_area[n] <- round(total_area - sum(norm_area[-n]), 2)
  norm_production[n] <- round(total_production - sum(norm_production[-n]), 2)
  
  # Δημιουργία πίνακα δεδομένων
  data <- data.frame(
    Field = combinations$Field,
    Crop = combinations$Crop,
    Area = norm_area,
    Production = norm_production
  )
  
  return(data)
}
set.seed(1)  # Για μη εναλλασσόμενα αποτελέσματα
result <- agroDataGen(num_fields = 3, num_crops = 2, total_area = 100, total_production = 5000)
print(result)
# Έλεγχος συνολικών τιμών
cat("Συνολική έκταση:", sum(result$Area), "\n")
cat("Συνολική παραγωγή:", sum(result$Production), "\n")

ΜΕ ΤΡΕΞΙΜΟ ΤΟΥ ΚΩΔΙΚΑ
agroDataGen <- function(num_fields, num_crops, total_area, total_production) {
+     # Λίστα διαθέσιμων καλλιεργειών
+     crops <- c("Wheat", "Corn", "Barley", "Sunflower", "Cotton",
+                "Tomato", "Potato", "Rice")
+     
+     # Τυχαία επιλογή των καλλιεργειών που θα χρησιμοποιηθούν
+     selected_crops <- sample(crops, num_crops)
+     
+     # Δημιουργία κωδικών για αγροτεμάχια (F1, F2, ...)
+     fields <- paste0("F", 1:num_fields)
+     
+     # Δημιουργία όλων των συνδυασμών αγροτεμαχίου-καλλιέργειας
+     combinations <- expand.grid(Field = fields, Crop = selected_crops, stringsAsFactors = FALSE)
+     
+     # Πλήθος γραμμών του πίνακα (αγροτεμάχιο x καλλιέργεια)
+     n <- nrow(combinations)
+     
+     # Τυχαίες αρχικές τιμές (ομοιόμορφα κατανεμημένες)
+     raw_area <- runif(n)
+     raw_production <- runif(n)
+     
+     # Κανονικοποίηση για να τηρηθούν τα όρια του χρήστη
+     norm_area <- round(raw_area / sum(raw_area) * total_area, 2)
+     norm_production <- round(raw_production / sum(raw_production) * total_production, 2)
+     
+     # Διόρθωση τελευταίας τιμής ώστε να είναθ ίση με τα συνολικά όρια
+     norm_area[n] <- round(total_area - sum(norm_area[-n]), 2)
+     norm_production[n] <- round(total_production - sum(norm_production[-n]), 2)
+     
+     # Δημιουργία πίνακα δεδομένων
+     data <- data.frame(
+         Field = combinations$Field,
+         Crop = combinations$Crop,
+         Area = norm_area,
+         Production = norm_production
+     )
+     
+     return(data)
+ }
> set.seed(1)  # Για μη εναλλασσόμενα αποτελέσματα
> result <- agroDataGen(num_fields = 3, num_crops = 2, total_area = 100, total_production = 5000)
> print(result)
  Field      Crop  Area Production
1    F1     Wheat 13.68    1466.77
2    F2     Wheat 21.69     144.05
3    F3     Wheat  4.82     480.23
4    F1 Sunflower 21.46     411.64
5    F2 Sunflower 22.56    1601.78
6    F3 Sunflower 15.79     895.53
> # Έλεγχος συνολικών τιμών
> cat("Συνολική έκταση:", sum(result$Area), "\n")
Συνολική έκταση: 100 
> cat("Συνολική παραγωγή:", sum(result$Production), "\n")
Συνολική παραγωγή: 5000 
> 
