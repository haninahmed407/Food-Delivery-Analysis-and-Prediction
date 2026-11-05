})
output$prep_time_hist <- renderPlot({
df <- data_clean_and_engineer(); req(df)
ggplot(df, aes(x = Preparation_Time_min)) + geom_histogram(binwidth = 2,fill="lightgreen",color="darkgreen") +
labs(title = "Preparation Time Distribution", x = "Preparation Time (min)", y = "Number of Orders / Frequency") + theme_minimal()
})
output$kmeans_plot <- renderPlot({
km_data <- km_result_reactive(); req(km_data)
fviz_cluster(km_data$km_model, data = km_data$data_scaled, geom = "point", ellipse.type = "convex", ggtheme = theme_minimal())
})
output$Decision_tree_plot<-renderPlot({
rpart.plot(tree_model,
type=4,
extra=101,
fallen.leaves = TRUE,
main="شجرة القرار للتنبؤ بالتسليم المتاخر")
})
output$weather_bar_plot <- renderPlot({
df <- data_clean_and_engineer(); req(df)
ggplot(df, aes(x = Weather, fill = Weather)) +
geom_bar() +
labs(title = "Order Distribution by Weather Condition", x = "Weather Condition", y = "Frequency") + theme_minimal()
})
output$time_of_day_boxplot <- renderPlot({
df <- data_clean_and_engineer(); req(df)
ggplot(df, aes(x = Time_of_Day, y = Delivery_Time_min, fill = Time_of_Day)) +
geom_boxplot() +
labs(title = "Delivery Time Distribution by Time of Day", x = "Time of Day", y = "Delivery Time (min)") + theme_minimal()
})
}
shinyApp(ui = ui, server = server)
#DATA CLEANING
library(dplyr)
file_path<-file.choose()
data<-read.csv(file_path)
str(data)
#ظهر انواع الداتا و حجمها
#data cleaning
#بيعد كام قيمه مفقوده في العمود
data_clean<-na.omit(data)
colSums(is.na(data_clean))
duplicated(data_clean)
sum(is.na(data_clean))
# الخطوه الجايه اننا نزيل القيم المتطرفه نحدد الحدود القصوى و الدنيا
#q1=النقطه اللي 25 في الميه من البيانات اقل منها
#q2=50 في الميه
#q3=75 في الميه
#IQR=q3-q1
#الحد الادني =q1-(1.5*IQR)
#الحد الاعلى =Q3-(1.5*IQR)
#اي قيمه اقل او كبر من الحدود دي بتكون متطرفه
#بنستخدم quantile()لحساب اي نسبه مئويه
q1_delivery<-quantile(data_clean$Delivery_Time_min,0.25,na.rm=TRUE)
#استخدمنا na.rm=true زياده احتياط بالرغم اننا بالفعل شيلنا الاعمده الفارغه
#مستخدمناش q2عشان ده بيقيس تمركز البيانات و ليس مدى تشتتها
q3_delivery<-quantile(data_clean$Delivery_Time_min,0.75,na.rm=TRUE)
# علامه $ دي للوصول الي عمود محدد او متغير داخل اطار بيانات
IQR_delivery<-q3_delivery-q1_delivery
upper_bound_delivery<-q3_delivery+1.5*IQR_delivery
lower_bound_delivery<-q1_delivery-1.5*IQR_delivery
data_clean_no_outliers<-data_clean%>%filter(Delivery_Time_min>=lower_bound_delivery&Delivery_Time_min<=upper_bound_delivery)
# علامه %>% معناها ثم
Q1_dist <- quantile(data_clean_no_outliers$Distance_km, 0.25, na.rm = TRUE)
Q3_dist <- quantile(data_clean_no_outliers$Distance_km, 0.75, na.rm = TRUE)
IQR_dist <- Q3_dist - Q1_dist
upper_bound_dist <- Q3_dist + 1.5 * IQR_dist
lower_bound_dist <- Q1_dist - 1.5 * IQR_dist
# التصفية لحذف القيم المتطرفة من Distance_km
data_final_clean <- data_clean_no_outliers %>%filter(Distance_km>=lower_bound_dist &Distance_km<= upper_bound_dist)
#اخر مرحله من ال DATA CLEANING هي انشاء ميزات جديده
#______________________________________________________________________________________________________
#feature engineering
#speed=distance_km/delievry_time_min
library(tidyverse)
data_final_clean<-data_final_clean%>%mutate(Speed_km_per_min=Distance_km/Delivery_Time_min)
#mutate هي function  معناها اضف عمود جديد اسمه speed و قيمته هي ناتج قسمة المسافه على الزمن
data_final_clean<-data_final_clean%>%mutate(Late_Delivery=ifelse(Delivery_Time_min>q3_delivery,1,0))
#استخدمنا ifelse مع mutate لفحص كل صف في كل عمود
#كده عملنا عمود تصنيفي يحتوي على 0 و 1 بس
#عملنا late_delievry_flag
# تحويل إلى Factor
data_final_clean <- data_final_clean %>%mutate(
Order_ID = as.factor(Order_ID),
Weather = as.factor(Weather),
Traffic_Level = as.factor(Traffic_Level),
Time_of_Day = as.factor(Time_of_Day),
Vehicle_Type = as.factor(Vehicle_Type),
Distance_km = as.numeric(Distance_km),
Late_Delivery=as.factor(Late_Delivery))
str(data_final_clean)
#كده خلصنا data cleaning#
saveRDS(data_final_clean, "data_final_clean.rds")
#_________________________________________________________________________________________________________________________________
#مرحله ال visualizatio
library(ggplot2)
#مكتبه اساسيه علشان تعمل رسوم بيانيه
ggplot(data = data_final_clean, aes(x = Delivery_Time_min)) +
geom_histogram(binwidth = 5, fill = "blue", color = "black") +
labs(
title = "توزيع أوقات التوصيل  ",
x = "وقت التوصيل (دقيقة)",
y = "عدد الطلبات / التكرار"
)+theme_minimal()
# ده بيحدد اكتر وقت تقع فيه عمليات التوصيل ده بنعرفه من العمود الاطول
#شكل الرسم نفسه يحدد سرعه التوصيل
#ggplot تنشئ اطار فارغ للرسم البياني
#data_final_clean استخدمنا اخر داتا نضفناها
#aes(x=Delievry_Time_min) هنا خلينا R  تعمل العنوان ده على محور اكس الافقي
#geom_histogam لتحديد نوع الرسم
#binwidth=5 نحدد بيها عرض الفئه كل عمود هيمثل فاصل زمني مدته خمس دقايق
#fill="blue" لون ازرق لتحديد لون ملئ الاعمده
#color="black" تحديد لون الاطار الخارجي لكل عمود
#+بنربط بيها طبقات الرسم مع بعض
#labs()مخصص ل التسميات و العنوانين
#title بنستخدمه لوضع عنوان رئيسي للرسم
#theme_minimal()تنظيف المظهر و تعديله
ggplot(data = data_final_clean, aes(x = Vehicle_Type, y = Delivery_Time_min,fill=Vehicle_Type)) +
geom_boxplot() +
labs(
title = "مقارنة أوقات التوصيل حسب نوع المركبة",
x = "نوع المركبة",
y = "وقت التوصيل (دقيقة)",
fill="المركبه"
) +
theme_minimal()
#عباره عن صندوق لكل نوع مركبه يحدد سرعتها
str(data_final_clean)
#اضافه عمود تقييم لراي العملاء
data_final_clean$Customer_Rating <- factor(
sample(1:5, size = nrow(data_final_clean), replace = TRUE),
levels = 1:5
)
#نتاكد من ان العمود الجديد موجود
str(data_final_clean)
ggplot(data = data_final_clean,
aes(x = Customer_Rating, y = Delivery_Time_min, fill = Customer_Rating)) +
geom_boxplot() +
labs(
title =("تقييم العملاء بالنسبه لوقت التوصيل") ,
x = ("تقييم العميل") ,
y = "وقت التوصيل (دقيقة)",
fill = "التقييم"
) +
theme_minimal()
ggplot(data = data_final_clean, aes(x = Distance_km)) +
geom_histogram(binwidth = 1, fill = "red", color = "black") +
# 'binwidth=1' يجعل الأعمدة تمثل كل 1 كم
labs(
title = ("المسافه بين المطعم و العميل "),
x = "المسافة بالكيلومتر (km)",
y = "عدد الطلبات (Frequency)"
) +
theme_minimal()
ggplot(data = data_final_clean, aes(x = Weather, fill = Weather)) +
geom_bar() +
labs(
title = "7. توزيع عدد الطلبات حسب حالة الطقس",
x = "حالة الطقس",
y = "عدد الطلبات (Frequency)"
) +
theme_minimal()
ggplot(data = data_final_clean,
aes(x = Time_of_Day, y = Delivery_Time_min, fill = Time_of_Day)) +
geom_boxplot() +
labs(
title = "10. توزيع وقت التوصيل حسب الفترة الزمنية من اليوم",
x = "فترة اليوم",
y = "وقت التوصيل (دقيقة)"
) +
theme_minimal()
ggplot(data = data_final_clean,
aes(x = Distance_km, y = Delivery_Time_min)) +
geom_point(alpha = 0.6, color = "green") + # geom_point لعرض كل نقطة بيانات
geom_smooth( color = "red") + # لإضافة خط الانحدار الخطي
labs(
title = "9. العلاقة بين المسافة ووقت التوصيل",
x = "المسافة بالكيلومتر (Distance_km)",
y = "وقت التوصيل بالدقائق (Delivery_Time_min)"
) +
theme_minimal()
ggplot(data = data_final_clean, aes(x = Preparation_Time_min)) +
geom_histogram(binwidth = 2, fill = "darkgreen", color = "black") +
labs(
title = "8. توزيع أوقات إعداد الطلبات",
x = "وقت الإعداد (دقيقة)",
y = "عدد الطلبات / التكرار"
) +
theme_minimal()
#خلصنا visualization
#_____________________________________________________________________________________________________________
#cluster
library(cluster) # مكتبة أساسية للتجميع
library(factoextra) # مكتبة لعرض النتائج بيانيًا وتحديد K الأمثل
library(scales) #توحيد مقاييس البيانات
clustering_data <- data_final_clean %>%
select(Distance_km, Delivery_Time_min, Preparation_Time_min)
#ده استخدمناه عشان نوحد مقاييس البيانات
data_scaled <- scale(clustering_data)
set.seed(42) # لتثبيت النقطه اللي هيبدا من عندها كل مره
km_result <- kmeans(data_scaled, centers = 3)
#الداله اللي بتبني النموذج
data_final_clean$Cluster <- as.factor(km_result$cluster)
#بيضيف عمود يوضح فيه كل مجموعه للداتا بتاعتنا
# عرض المجموعات الناتجة بيانيًا
fviz_cluster(km_result, data = data_scaled,
palette = c("pink", "yellow", "orange"),
geom = "point",
ellipse.type = "convex",
ggtheme = theme_minimal()
)
saveRDS(km_result, "kmeans_model.rds")
saveRDS(data_scaled, "data_scaled.rds")
#كده خلصنا cluster
#__________________________________________________________________________________________________________________
#نبدا tree
library(rpart)
library(rpart.plot)
library(caret)
set.seed(42)
index <- createDataPartition(data_final_clean$Late_Delivery, p = 0.7, list = FALSE)
train_data <- data_final_clean[index, ]
test_data <- data_final_clean[-index, ]
tree_model <- rpart(Late_Delivery ~ Distance_km + Vehicle_Type + Traffic_Level + Preparation_Time_min,
data = train_data,
method = "class",
cp=0.03)
rpart.plot(tree_model, type = 4,extra = 101,  fallen.leaves = TRUE, main = "Decision Tree for Late Delivery Prediction")
predictions <- predict(tree_model, test_data, type = "class")
predictions_corrected<-factor(predictions,levels=levels(test_data$Late_Delivery))
confusion_matrix <- confusionMatrix(predictions_corrected, test_data$Late_Delivery)
print(confusion_matrix)
saveRDS(tree_model, "decision_tree_model.rds")
#كده خلصنا tree
#_____________________________________________________________________________________________________________________________
# gui
# app_full_gui.R
# نسخة مُجمعة ومحسّنة من الـ Shiny app — جاهزة للتشغيل
# لو ناقص عندك باكدجات شغلي: install.packages(c("shiny","dplyr","ggplot2","factoextra","rpart","caret"))
library(shiny)
library(dplyr)
library(ggplot2)
library(factoextra)
library(rpart)
library(caret)
library(rpart.plot)
# ==== دوال أمان لقراءة الملفات المحفوظة ====
safe_read <- function(path) {
if (file.exists(path)) tryCatch(readRDS(path), error = function(e) NULL) else NULL
}
# حاول يقرأ النماذج والداتا المحفوظة لو موجودة
tree_model <- safe_read("decision_tree_model.rds")
km_model   <- safe_read("kmeans_model.rds")
data_final_clean_base <- safe_read("data_final_clean.rds")
# لو مفيش داتا محفوظة — نولّد داتا اختبارية صغيرة عشان الواجهة ماتنهارش
#•	هذا الكود ينشئ بيانات تجريبية كاملة إذا البيانات الحقيقية مش موجودة.
#•	كل عمود له وظيفة: مسافة، وقت توصيل، وقت تحضير، نوع مركبة، ازدحام، طقس، وقت اليوم، تقييم العميل، وتصنيف متأخر/في الموعد.
#•	الهدف: تشغيل التطبيق بدون أخطاء حتى لو مفيش بيانات أصلية
if (is.null(data_final_clean_base) || nrow(data_final_clean_base) == 0) {
set.seed(123)
data_final_clean_base <- data.frame(
Order_ID = factor(1:30),
Distance_km = runif(30, 0.5, 20),
Delivery_Time_min = runif(30, 8, 120),
Preparation_Time_min = runif(30, 3, 45),
Vehicle_Type = factor(sample(c("Bike","Car","Motor"), 30, replace = TRUE)),
Traffic_Level = factor(sample(c("Low","Medium","High"), 30, replace = TRUE)),
Weather = factor(sample(c("Sunny","Rainy","Cloudy"), 30, replace = TRUE)),
Time_of_Day = factor(sample(c("Morning","Afternoon","Evening"), 30, replace = TRUE))
)
q3_tmp <- quantile(data_final_clean_base$Delivery_Time_min, 0.75)
data_final_clean_base$Late_Delivery <- factor(ifelse(data_final_clean_base$Delivery_Time_min > q3_tmp, "Late", "On_Time"))
data_final_clean_base$Customer_Rating <- factor(sample(1:5, 30, replace = TRUE))
}
vehicle_levels_init <- if (nlevels(data_final_clean_base$Vehicle_Type) > 0) levels(data_final_clean_base$Vehicle_Type) else c("Bike","Car")
traffic_levels_init <- if (nlevels(data_final_clean_base$Traffic_Level) > 0) levels(data_final_clean_base$Traffic_Level) else c("Low","Medium","High")
# ==== UI ====
ui <- fluidPage(
titlePanel("نظام التنبؤ والتصنيف والتحليل — نسخة مجمعة"),
tabsetPanel(
tabPanel("التنبؤ والتصنيف (GUI)",
sidebarLayout(
sidebarPanel(
fileInput("file_upload", "تحميل ملف بيانات جديد (CSV):",
multiple = FALSE, accept = c(".csv","text/csv")),
tags$hr(),
h4("إدخال بيانات طلب للتنبؤ:"),
numericInput("dist", "المسافة (كم):", value = 5, min = 0),
selectInput("vehicle", "نوع المركبة:", choices = vehicle_levels_init),
selectInput("traffic", "مستوى الازدحام:", choices = traffic_levels_init),
numericInput("prep_time", "وقت الإعداد (دقيقة):", value = 20, min = 0),
actionButton("predict_btn", "توقع / صنف الطلب"),
tags$hr(),
h4("K-Means:"),
numericInput("k_clusters", "عدد المجموعات (K):", value = 3, min = 2, max = 10),
actionButton("run_kmeans_btn", "تطبيق التجميع")
),
mainPanel(
h3("نتائج التحليل:", style = "color: #1F78B4;"),
verbatimTextOutput("prediction_result"),
verbatimTextOutput("cluster_result")
)
)
),
tabPanel("الرسومات التفصيلية",
fluidRow(
column(6, h4("1. توزيع أوقات التوصيل"), plotOutput("delivery_time_hist", height = "300px")),
column(6, h4("2. التوصيل حسب نوع المركبة"), plotOutput("vehicle_boxplot", height = "300px"))
),
fluidRow(
column(6, h4("3. توزيع المسافة (Histogram)"), plotOutput("distance_hist", height = "300px")),
column(6, h4("4. تقييم العميل vs وقت التوصيل"), plotOutput("rating_boxplot", height = "300px"))
),
fluidRow(
column(6, h4("5. المسافة مقابل وقت التوصيل (Scatter + Smooth)"), plotOutput("dist_vs_time", height = "350px")),
column(6, h4("6. وقت الإعداد (Histogram)"), plotOutput("prep_time_hist", height = "350px"))
),
fluidRow(
column(6,h4("7.الطلبات حسب الطقس"),
plotOutput("weather_bar_plot",height="350px")),
column(6,h4("8.وقت التوصيل حسب اليوم "),
plotOutput("time_of_day_boxplot",height="350px"))
)
),
tabPanel(("نتائج التجميع (Clustering)"),
h4("K-Means Plot"),
plotOutput("kmeans_plot", height = "600px"))
,
tabPanel(("(Decision tree)نموذج شجرة القرار"),
h4("شجرة القرار للتنبؤ بالتسليم المتاخر"),
plotOutput("Decision_tree_plot",height="800px")
)
))
# ==== Server ====
server <- function(input, output, session) {
# دالة تجهز الداتا: قراءة، تنظيف، انشاء ميزات، وتحديث اختيارات الـ select
data_clean_and_engineer <- reactive({
# قراءة الملف المرفوع أو استخدام الداتا الأساسية
if (is.null(input$file_upload)) {
df <- data_final_clean_base
showNotification("يتم استخدام البيانات المحفوظة/الاختبارية.", type = "message", duration = 2)
} else {
df_raw <- tryCatch(read.csv(input$file_upload$datapath, header = TRUE, stringsAsFactors = FALSE),
error = function(e) { showNotification("خطأ في قراءة CSV.", type = "error"); return(NULL) })
req(df_raw)
df <- df_raw %>% na.omit()
}
# فحص الأعمدة المطلوبة
required_cols <- c("Distance_km","Delivery_Time_min","Preparation_Time_min","Vehicle_Type","Traffic_Level")
missing_cols <- setdiff(required_cols, names(df))
if (length(missing_cols) > 0) {
showNotification(paste0("الملف يفتقد الأعمدة: ", paste(missing_cols, collapse = ", ")), type = "error", duration = NULL)
return(NULL)
}
# إزالة outliers بناءً على IQR
q1_delivery <- quantile(df$Delivery_Time_min, 0.25, na.rm = TRUE)
q3_delivery <- quantile(df$Delivery_Time_min, 0.75, na.rm = TRUE)
IQR_delivery <- q3_delivery - q1_delivery
df <- df %>% filter(Delivery_Time_min >= (q1_delivery - 1.5*IQR_delivery) & Delivery_Time_min <= (q3_delivery + 1.5*IQR_delivery))
q1_dist <- quantile(df$Distance_km, 0.25, na.rm = TRUE)
q3_dist <- quantile(df$Distance_km, 0.75, na.rm = TRUE)
IQR_dist <- q3_dist - q1_dist
df <- df %>% filter(Distance_km >= (q1_dist - 1.5*IQR_dist) & Distance_km <= (q3_dist + 1.5*IQR_dist))
# feature engineering
df <- df %>%
mutate(
Speed_km_per_min = Distance_km / Delivery_Time_min,
Late_Delivery = factor(ifelse(Delivery_Time_min > q3_delivery, "Late", "On_Time"))
)
# تحويل الأنواع
df <- df %>%
mutate(
Order_ID = if ("Order_ID" %in% names(df)) as.factor(Order_ID) else as.factor(1:n()),
Vehicle_Type = as.factor(Vehicle_Type),
Traffic_Level = as.factor(Traffic_Level),
Distance_km = as.numeric(Distance_km),
Preparation_Time_min = as.numeric(Preparation_Time_min)
)
if (!"Customer_Rating" %in% names(df)) {
df$Customer_Rating <- factor(sample(1:5, nrow(df), replace = TRUE), levels = 1:5)
} else {
df$Customer_Rating <- as.factor(df$Customer_Rating)
}
# حدث اختيارات الـ selectInput (عشان المستوى الجديد لو اترفع داتا مختلفة)
isolate({
if (nlevels(df$Vehicle_Type) > 0) updateSelectInput(session, "vehicle", choices = levels(df$Vehicle_Type))
if (nlevels(df$Traffic_Level) > 0) updateSelectInput(session, "traffic", choices = levels(df$Traffic_Level))
})
return(df)
})
# K-means reactive
km_result_reactive <- eventReactive(input$run_kmeans_btn, {
df <- data_clean_and_engineer(); req(df)
clustering_data <- df %>% select(Distance_km, Delivery_Time_min, Preparation_Time_min)
data_scaled <- scale(clustering_data)
set.seed(42)
km <- kmeans(data_scaled, centers = input$k_clusters, nstart = 25)
showNotification(paste0("تم تطبيق K-Means على ", input$k_clusters, " مجموعات."), type = "message")
return(list(km_model = km, data_scaled = data_scaled))
}, ignoreNULL = FALSE)
# زر التنبؤ
observeEvent(input$predict_btn, {
df <- data_clean_and_engineer(); req(df)
vehicle_levels <- levels(df$Vehicle_Type); traffic_levels <- levels(df$Traffic_Level)
new_data <- data.frame(
Distance_km = as.numeric(input$dist),
Vehicle_Type = factor(input$vehicle, levels = vehicle_levels),
Traffic_Level = factor(input$traffic, levels = traffic_levels),
Preparation_Time_min = as.numeric(input$prep_time)
)
# لو النموذج مش موجود، نعلم المستخدم
if (is.null(tree_model)) {
output$prediction_result <- renderText("خطأ: نموذج الشجرة غير موجود. درّبي النموذج واحفظيه كـ decision_tree_model.rds في نفس المجلد.")
return(NULL)
}
pred_text <- tryCatch({
pred_raw <- predict(tree_model, new_data, type = "class")
pred_char <- as.character(pred_raw)
# ترجمة للنتائج الشائعة
pred_friendly <- ifelse(
pred_char %in% c("1","1.0","TRUE","Late","late","LATE"), "متأخر",
ifelse(pred_char %in% c("0","0.0","FALSE","On_Time","On time","OnTime","On_Time"), "في الموعد", pred_char)
)
paste0("التنبؤ بالتسليم: ", pred_friendly)
}, error = function(e) {
paste0("حدث خطأ أثناء التنبؤ: ", e$message)
})
output$prediction_result <- renderText({ pred_text })
output$cluster_result <- renderText({ "اضغطي 'تطبيق التجميع' لرؤية نتائج K-Means (إن رغبت)." })
})
# ---- الرسومات ----
output$delivery_time_hist <- renderPlot({
df <- data_clean_and_engineer(); req(df)
ggplot(df, aes(x = Delivery_Time_min)) +
geom_histogram(binwidth = 5,fill="lightblue",color="darkblue") +
labs(title = "توزيع أوقات التوصيل", x = "وقت التوصيل (دقيقة)", y = "تكرار") + theme_minimal()
})
output$vehicle_boxplot <- renderPlot({
df <- data_clean_and_engineer(); req(df)
ggplot(df, aes(x = Vehicle_Type, y = Delivery_Time_min, fill = Vehicle_Type)) +
geom_boxplot() +
labs(title = "أوقات التوصيل حسب نوع المركبة", x = "نوع المركبة", y = "وقت التوصيل (دقيقة)") + theme_minimal()
})
output$distance_hist <- renderPlot({
df <- data_clean_and_engineer(); req(df)
ggplot(df, aes(x = Distance_km)) + geom_histogram(binwidth = 1,fill="lightcoral",color="darkred") +
labs(title = "توزيع المسافة (كم)", x = "المسافة (كم)", y = "تكرار") + theme_minimal()
})
output$rating_boxplot <- renderPlot({
df <- data_clean_and_engineer(); req(df)
ggplot(df, aes(x = Customer_Rating, y = Delivery_Time_min, fill = Customer_Rating)) +
geom_boxplot() + labs(title = "تقييم العميل بالنسبة لوقت التوصيل", x = "تقييم العميل", y = "وقت التوصيل (دقيقة)") + theme_minimal()
})
output$dist_vs_time <- renderPlot({
df <- data_clean_and_engineer(); req(df)
ggplot(df, aes(x = Distance_km, y = Delivery_Time_min)) +
geom_point(alpha = 0.6,color="purple") +
geom_smooth(method = "loess", se = TRUE,color="orange") +
labs(title = "المسافة مقابل وقت التوصيل", x = "المسافة (كم)", y = "وقت التوصيل (دقيقة)") + theme_minimal()
})
output$prep_time_hist <- renderPlot({
df <- data_clean_and_engineer(); req(df)
ggplot(df, aes(x = Preparation_Time_min)) + geom_histogram(binwidth = 2,fill="lightgreen",color="darkgreen") +
labs(title = "توزيع أوقات إعداد الطلبات", x = "وقت الإعداد (دقيقة)", y = "تكرار") + theme_minimal()
})
output$kmeans_plot <- renderPlot({
km_data <- km_result_reactive(); req(km_data)
fviz_cluster(km_data$km_model, data = km_data$data_scaled, geom = "point", ellipse.type = "convex", ggtheme = theme_minimal())
})
output$Decision_tree_plot<-renderPlot({
rpart.plot(tree_model,
type=4,
extra=101,
fallen.leaves = TRUE,
main="شجرة القرار للتنبؤ بالتسليم المتاخر")
})
output$weather_bar_plot <- renderPlot({
df <- data_clean_and_engineer(); req(df)
ggplot(df, aes(x = Weather, fill = Weather)) +
geom_bar() +
labs(title = "توزيع عدد الطلبات حسب حالة الطقس", x = "حالة الطقس", y = "تكرار") + theme_minimal()
})
output$time_of_day_boxplot <- renderPlot({
df <- data_clean_and_engineer(); req(df)
ggplot(df, aes(x = Time_of_Day, y = Delivery_Time_min, fill = Time_of_Day)) +
geom_boxplot() +
labs(title = "وقت التوصيل حسب الفترة الزمنية من اليوم", x = "فترة اليوم", y = "وقت التوصيل (دقيقة)") + theme_minimal()
})
}
shinyApp(ui = ui, server = server)
library(shiny); runApp('D:/Hanin/arabic code.R')
install.packeges("dpylr")
runApp('D:/Hanin/englich code.R')

library(shiny); runApp('D:/Hanin/final data project.R')
runApp('D:/Hanin/final data project.R')

library(shiny); runApp('D:/Hanin/final data project.R')
runApp('D:/Hanin/final data project.R')
