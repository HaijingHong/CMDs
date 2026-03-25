###-----------R包检验、安装、加载-----------
if (!require("dplyr")) install.packages("dplyr");library("dplyr") #数据处理
if (!require("lubridate")) install.packages("lubridate");library("lubridate") #ymd()
if (!require("haven")) install.packages("haven");library("haven") #去标签
if (!require("ggplot2")) install.packages("ggplot2");library("ggplot2") #画图
if (!require("lcmm")) install.packages("lcmm");library("lcmm") #LCGM模型hlme()、summarytable()
if (!require("purrr")) install.packages("purrr");library("purrr") #平均后验概率:imap_dfr()、map()
if (!require("devtools")) install.packages("devtools");library("devtools")
if (!require("LCTMtools")) devtools::install_github("hlennon/LCTMtools");library("LCTMtools") #LCTMtools
if (!require("psych")) install.packages("psych");library("psych") # correlation analysis
if (!require("corrplot")) install.packages("corrplot");library("corrplot")  # correlation heatmaps
if (!require("dlookr")) install.packages("dlookr");library("dlookr") # missing-data visualization, plots
if (!require("officer")) install.packages("officer");library("officer") # export to Word
if (!require("flextable")) install.packages("flextable");library("flextable") # export to Word
if (!require("missForest")) install.packages("missForest");library("missForest") # missing-value imputation: missForest()
if (!require("caret")) install.packages("caret");library("caret") # data split: createDataPartition(); scaling: preProcess()
if (!require("tidymodels")) install.packages("tidymodels");library("tidymodels")  #smotenc函数：recipe()
if (!require("themis")) install.packages("themis");library("themis") #smotenc函数：step_smotenc()
if (!require("mlr3")) install.packages("mlr3");library("mlr3") # core ML framework
if (!require("mlr3verse")) install.packages("mlr3verse");library("mlr3verse") # mlr3 ecosystem extensions
if (!require("mlr3extralearners")) devtools::install_github("https://github.com/mlr-org/mlr3extralearners"); library("mlr3extralearners")  # extra algorithms for mlr3
if (!require("ranger")) install.packages("ranger");library("ranger") # random forest (mlr3 does not bundle ranger)
if (!require("xgboost")) install.packages("xgboost");library("xgboost") # XGBoost (mlr3 does not bundle xgboost)
if (!require("e1071")) install.packages("e1071");library("e1071") # SVM (mlr3 does not bundle e1071)
if (!require("mlr3learners")) install.packages("mlr3learners");library("mlr3learners") #Algorithm packages
library(mlr3tuning) #超参数优化包
if (!require("kknn")) install.packages("kknn");library("kknn") # knn (mlr3 does not bundle knn)
if (!require("pROC")) install.packages("pROC");library("pROC") # ROC curves with 95% CI
if (!require("patchwork")) install.packages("patchwork");library("patchwork") # combine ggplot figures
if (!require("rmda")) install.packages("rmda");library("rmda") # decision curve analysis
if (!require("kernelshap")) install.packages("kernelshap");library("kernelshap") # SHAP value computation
if (!require("shapviz")) install.packages("shapviz");library("shapviz") # SHAP visualizations
if (!require("gtsummary")) install.packages("gtsummary");library("gtsummary") # baseline characteristics tables
if (!require("cardx")) install.packages("cardx");library("cardx") # baseline characteristics tables
if (!require("rmda")) install.packages("rmda");library("rmda") # decision curve analysis
if (!require("riskRegression")) install.packages("riskRegression");library("riskRegression") # calibration plots
if (!require("tidyverse")) install.packages("tidyverse");library("tidyverse") # data extraction
if (!require("ggradar")) devtools::install_github("ricardo-bion/ggradar");library("ggradar") # radar charts
if (!require("cowplot")) install.packages("cowplot");library("cowplot") #拼图
if (!require("mice")) install.packages("mice");library("mice") #multiple interpolation
if (!require("ggsankey")) devtools::install_github("davidsjoberg/ggsankey");library("ggsankey") #绘制桑基图

# shiny app dependencies、 cloud deployment
install.packages(c("shiny", "DT", "plotly", "iml", "shinydashboard", "shinyjs", "progressr", "rsconnect"))
library(shiny)


#导入数据
load("D:/graduate documents/prediction model2/mj03_24_yuce.RData")

#筛选2017-2024年数据
mj17_24 <- mj03_24_yuce %>%
  filter(exdate >= as.Date("2017-01-01"),
         exdate <= as.Date("2024-12-31"))

#转换为数据框
mj <- as.data.frame(mj17_24)

# 去掉 haven 的标签属性
mj <- haven::zap_labels(mj)
mj <- haven::zap_label(mj)
mj[] <- lapply(mj, function(x) {
  if (inherits(x, "haven_labelled")) unclass(x) else x
})

#创建变量名映射表
variable_dict <- data.frame(
  raw_name = c(
    "accountid","exdate","age","sex","scholar","occupation","marriage",
    "yearincome","BloodType","smokeornot","drinkornot","foodtime",
    "sleeptime","FirstSportFrequ","SameAgeHealth","nopsick","psick01",
    "psick02","psick03","psick04","psick05","psick06","psick07",
    "psick08","psick25","Cancer","psick10","psick23","psick14",
    "psick11","psick12","psick13","psick15","psick16","psick17",
    "psick18","Liver diseases","psick19","psick20","psick21",
    "psick22","ESLLS","ESRLS","Vision","LHearing","RHearing",
    "Hearing","PressSR1","PressSR2","Height","Weight","BMI",
    "WaWidth","HipWidth","BodyFat","UrEw","UrS","Insulin","BloodDep",
    "HS_CRP","Ushba1c2","BgChol","BgHdlc","BgLdlc","BgTg",
    "UfBun","Ushba1c1","UfUa","BloodWBC","BloodHB","BloodMCV",
    "BloodPLT","BloodHCT","UfCre"
  ),
  analysis_name = c(
    "accountid","exdate","Age","sex","Education","Occupation","Marital_status",
    "Personal_annual_income","Blood_type","Smoking","Drinking","Regular_eating",
    "Nighttime_sleep","Exercise_frequency","Self_rated_health","nopsick","psick01",
    "psick02","psick03","psick04","psick05","psick06","psick07",
    "psick08","psick25","Cancer","DM","Arthritis","Asthma",
    "Cerebrovascular_diseases","Cardiovascular_diseases","Thyroid_diseases",
    "Tuberculosis","Digestive_diseases","Hepatitis","Cirrhosis",
    "Liver_diseases","Kidney_diseases","Urolithiasis","Gout",
    "Anemia","ESLLS","ESRLS","Vision","LHearing","RHearing",
    "Hearing","SBP","DBP","Height","Weight","BMI",
    "Waist_circumference","Hip_circumference","BFP","Urine_protein","Urine_glucose",
    "Insulin","SpO2","HS_CRP","HbA1c","TC","HDL_C","LDL_C","TG",
    "BUN","FBG","UA","WBC","HB","MCV","PLT","HCT","Creatinine"
  ),
  display_name = c(
    "accountid","exdate","Age","Sex","Education","Occupation","Marital status",
    "Personal annual income","Blood type","Smoking","Drinking","Regular eating",
    "Nighttime sleep","Exercise frequency","Self-rated health","nopsick","psick01",
    "psick02","psick03","psick04","psick05","psick06","psick07",
    "psick08","psick25","Cancer","DM","Arthritis","Asthma",
    "Cerebrovascular diseases","Cardiovascular diseases","Thyroid diseases",
    "Tuberculosis","Digestive diseases","Hepatitis","Cirrhosis",
    "Liver diseases","Kidney diseases","Urolithiasis","Gout",
    "Anemia","ESLLS","ESRLS","Vision","LHearing","RHearing",
    "Hearing","SBP","DBP","Height","Weight","BMI",
    "Waist circumference","Hip circumference","BFP","Urine protein","Urine glucose",
    "Insulin","SpO2","HS-CRP","HbA1c","TC","HDL-C","LDL-C","TG",
    "BUN","FBG","UA","WBC","HB","MCV","PLT","HCT","Creatinine"
  ),
  stringsAsFactors = FALSE
)
#变量名映射表
idx <- match(names(mj), variable_dict$raw_name)
colnames(mj)[!is.na(idx)] <- variable_dict$analysis_name[idx[!is.na(idx)]]


###----------eGFR、CMDs编码----------
##年龄：连续变量；性别：1=Man，2=Woman
mj <- mj %>%
  mutate(
    Age = as.numeric(Age), #年龄列转为数值型
    Sex = as.numeric(sex), #性别列转为数值型并重命名为Sex
    Sex = factor(Sex, levels = c(1, 2), labels = c("Man", "Woman")) #转换为有标签因子型
  )


##----------计算 eGFR（CKD-EPI 2021, creatinine-only）
#肌酐转换为数值型
mj$Creatinine <- as.numeric(mj$Creatinine)  # 肌酐：单位 μmol/L

#肌酐Creatinine: NA=(排除生理不可能的值：20–1500 μmol/L), μmol/88.4→mg/dL
mj$Creatinine <- ifelse(mj$Creatinine < 20 | mj$Creatinine > 1500, NA, mj$Creatinine / 88.4)

#定义 CKD-EPI 2021 creatinine-only 公式
eGFR_2021 <- function(Scr_mgdl, age, sex) {
  k <- ifelse(sex == "Woman", 0.7, 0.9)        # 女0.7, 男0.9
  a <- ifelse(sex == "Woman", -0.241, -0.302)  # 女-0.241, 男-0.302
  C <- ifelse(sex == "Woman", 1.012, 1)        # 女1.012, 男1
  
  ratio <- Scr_mgdl / k
  
  eGFR <- 142 *
    (pmin(ratio, 1) ^ a) *
    (pmax(ratio, 1) ^ -1.200) *
    (0.9938 ^ age) *
    C
  
  return(eGFR)
}

#计算eGFR
mj$eGFR <- eGFR_2021(
  Scr_mgdl = mj$Creatinine,
  age      = mj$Age,
  sex      = mj$Sex
)

#eGFR: NA=(排除生理不可能的值：>150ml/min/1.73m²)
mj$eGFR <- ifelse(mj$eGFR > 150, NA, mj$eGFR)


##----------疾病变量编码：无病=1，有病=2
#定义疾病编码函数
encode01 <- function(x){
  x <- as.character(x)
  out <- rep(NA_real_, length(x))
  out[x %in% c("T", "TRUE")] <- 2  # 有病
  out[x %in% c("F", "FALSE")] <- 1 # 无病
  return(out)
}

mj$Kidney_diseases <- encode01(mj$Kidney_diseases) #肾病
mj$DM <- encode01(mj$DM) #糖尿病
mj$Cardiovascular_diseases <- encode01(mj$Cardiovascular_diseases) #心血管疾病（心脏病）
mj$Cerebrovascular_diseases <- encode01(mj$Cerebrovascular_diseases) #脑血管疾病（中风）


##----------找出每个 accountid 的首次体检 = baseline
#按accountid 分组，计算每个accountid的 exdate 最小值（忽略 NA）
baseline_info <- aggregate(
  exdate ~ accountid,
  data = mj,
  FUN = function(x) min(x, na.rm = TRUE)
)

#列名 exdate 重命名为 baseline_date
names(baseline_info)[names(baseline_info) == "exdate"] <- "baseline_date"
#提取年份并生成新列
baseline_info$baseline_year <- as.integer(format(baseline_info$baseline_date, "%Y"))

# 合并回 mj，得到 mj2
mj2 <- merge(
  mj,
  baseline_info,
  by = "accountid",
  all.x = TRUE
)

# 抽出每个 accountid 的基线那一行
baseline0 <- mj2[mj2$exdate == mj2$baseline_date, ]


###----------纳排：总人数N=117537----------
length(unique(baseline0$accountid))

##(1)基线年龄不缺失且≥45岁(n=42003)
baseline1 <- baseline0[!is.na(baseline0$Age) & baseline0$Age >= 45, ]
length(unique(baseline1$accountid))


##(2)从基线起 eGFR 非缺失次数 ≥3 次（= 基线 + ≥2 次随访）(n=12862)
# 先只保留 baseline1 中这些人，并且从各自 baseline_date 之后的所有记录
tmp_all <- mj2[
  mj2$accountid %in% baseline1$accountid &
    mj2$exdate >= mj2$baseline_date,
]

# 从基线起 eGFR 非缺失总次数
egfr_from_baseline <- aggregate(
  eGFR ~ accountid,
  data = tmp_all,
  FUN  = function(x) sum(!is.na(x))   
)

#列名 eGFR 重命名为 n_egfr_from_baseline
names(egfr_from_baseline)[names(egfr_from_baseline) == "eGFR"] <- "n_egfr_from_baseline"

# 只保留：从基线起 eGFR 总次数 ≥3 的accountid
ids_step2 <- egfr_from_baseline$accountid[egfr_from_baseline$n_egfr_from_baseline >= 3]

#从 baseline1 中筛选出 accountid 属于 ids_step2 的行，并保存为 baseline2。
baseline2 <- baseline1[baseline1$accountid %in% ids_step2, ]
length(unique(baseline2$accountid))


##(3)排除基线既往有 CMDs（糖尿病、中风、心血管病）(n=11170)
baseline3 <- baseline2[
  !is.na(baseline2$DM) & baseline2$DM == 1 & # 无糖尿病
    !is.na(baseline2$Cerebrovascular_diseases) & baseline2$Cerebrovascular_diseases == 1 & # 无中风
    !is.na(baseline2$Cardiovascular_diseases) & baseline2$Cardiovascular_diseases == 1 # 无心血管病
  , ]
length(unique(baseline3$accountid))


##(4)排除肾病（NS=2）或 eGFR <60 (n=10969)
baseline4 <- baseline3[
  !is.na(baseline3$Kidney_diseases) & baseline3$Kidney_diseases == 1 & # 无肾病
    !is.na(baseline3$eGFR) & baseline3$eGFR >= 60 # eGFR ≥60
  , ]
length(unique(baseline4$accountid))


#入组accountid
ids_step4 <- baseline4$accountid

##构建纵向随访数据：基线起的所有体检记录
#从mj2中筛选最终的accountid，并exdate>=baseline_date
mj3 <- mj2[
  mj2$accountid %in% ids_step4 &
    mj2$exdate >= mj2$baseline_date,
]

# 直接从日期重新“提取年份”，不要用之前的 year 变量
mj3$year          <- as.integer(format(mj3$exdate, "%Y"))
mj3$baseline_year <- as.integer(format(mj3$baseline_date, "%Y"))

# 相对基线的连续时间（年）
mj3$time <- as.numeric(
  difftime(mj3$exdate, mj3$baseline_date, units = "days")
) / 365.25


##(4)排除随访期间结局CMDs为NA的accountid (n=10967)
#CMDs_event：1=未发生，2=发生，NA=无法判断
mj3$CMDs_event <- with(mj3, ifelse(
  DM == 2 | Cardiovascular_diseases == 2 | Cerebrovascular_diseases == 2, 2L, # 任一有病
  ifelse(
    DM == 1 & Cardiovascular_diseases == 1 & Cerebrovascular_diseases == 1, 1L, # 三个都没病
    NA_integer_   # 其他情况（有 NA）
  )
))

#排除所有 CMDs_event 曾出现 NA 的 accountid
valid_ids <- unique(mj3$accountid[is.na(mj3$CMDs_event)])
final17_24 <- mj3[!mj3$accountid %in% valid_ids, ]
length(unique(final17_24$accountid))



###----------轨迹识别：LCGM----------
#accountid转换为数值型
final17_24$accountid <- as.numeric(final17_24$accountid)

##（1）模型拟合
#多项式阶数（二次）：首先建立1类基础模型
set.seed(2025)
LCGM1 <- hlme(
  fixed = eGFR ~ 1 + time + I(time^2), #模型的固定效应部分（模型的主要公式），设置为二次（线性：eGFR ~ 1 + time，三次：eGFR ~ 1 + time + I(time^2) + I(time^3)）
  ng = 1, #潜类别数
  subject = "accountid", #嵌套结构的主体id
  data = final17_24  #数据集名称
)

##2-5类模型
#用来存放每个LCGM
LCGMs <- list()
#LCGM循环语句
for (k in 2:5) {
  LCGMs[[paste0("LCGM", k)]] <- hlme(
    fixed = eGFR ~ 1 + time + I(time^2),
    mixture = ~ 1 + time + I(time^2), #混合效应：在类别数大于1的时候需要设置（即在不同的潜在类别中可能有所不同的模型参数）
    ng = k,
    subject = "accountid",
    data = final17_24,
    B = LCGM1 #初始模型
  )
}

###（2）模型拟合优度评估：AIC、BIC、entropy熵值、组占比、Avepp、OCC 
##①BIC(绝对值越低)、AIC(绝对值越低)、熵值(接近1)、组占比(>5%)
LCGM_result <- summarytable(LCGMs$LCGM2, LCGMs$LCGM3, LCGMs$LCGM4, LCGMs$LCGM5,
                            which = c("G", "AIC", "BIC", "SABIC", "entropy", "%class"))

#LCGM_result转换为数据框，取类占比最小值到新列
LCGM_result <- LCGM_result %>%
  as.data.frame() %>%
  mutate(
    `Smallest Class (%)` = pmin(`%class1`, `%class2`, `%class3`, `%class4`, `%class5`,
                                na.rm = TRUE)
  ) %>%
  select(-starts_with("%class")) %>%
  rename(Model = G, Entropy = entropy)

##②平均后验概率Avepp＞0.7；正确分类的优势(odds of correct classification，OCC)＞5
# 先定义各模型列表
LCGM_models <- list(
  LCGM2 = LCGMs$LCGM2,
  LCGM3 = LCGMs$LCGM3,
  LCGM4 = LCGMs$LCGM4,
  LCGM5 = LCGMs$LCGM5
)

# 对每个模型分别运行 LCTMtoolkit
Avepp_OCC_results_list <- lapply(names(LCGM_models), function(name) {
  cat("Model being processed:", name, "\n")
  mod <- LCGM_models[[name]]
  # 安全调用，出错时不会中断整个循环
  tryCatch({
    res <- LCTMtoolkit(mod)
    list(name = name, result = res)
  }, error = function(e) {
    cat("??? Model", name, "Error:", e$message, "\n")
    NULL
  })
})

#提取Avepp结果
appa_results <- bind_rows(Avepp_OCC_results_list[[1]][["result"]][["appa"]], 
                          Avepp_OCC_results_list[[2]][["result"]][["appa"]], 
                          Avepp_OCC_results_list[[3]][["result"]][["appa"]], 
                          Avepp_OCC_results_list[[4]][["result"]][["appa"]])

#appa_results取最小值到新列
appa_results <- appa_results %>%
  mutate(
    `Smallest avepp` = pmin(Class_1, Class_2, Class_3, Class_4, Class_5, na.rm = TRUE)
  )

#提取OCC结果
occ_results <- bind_rows(Avepp_OCC_results_list[[1]][["result"]][["occ"]], 
                         Avepp_OCC_results_list[[2]][["result"]][["occ"]], 
                         Avepp_OCC_results_list[[3]][["result"]][["occ"]], 
                         Avepp_OCC_results_list[[4]][["result"]][["occ"]])

#occ_results取最小值到新列
occ_results <- occ_results %>%
  mutate(
    `Smallest OCC` = pmin(Class_1, Class_2, Class_3, Class_4, Class_5, na.rm = TRUE)
  )

#合并全部结果
LCGM_results <- cbind(
  LCGM_result,
  `Smallest avepp` = appa_results[ , "Smallest avepp"],
  `Smallest OCC` = occ_results[ , "Smallest OCC"]
)

#保留两位小数
LCGM_results[ , -1] <- round(LCGM_results[ , -1], 2)

###（3）LCGM轨迹图
#构造时间网格
newdata <- data.frame(time = seq(
  from = min(final17_24$time, na.rm = TRUE),
  to   = max(final17_24$time, na.rm = TRUE),
  length.out = 100
))

#提取模型的预测值
plotpred <- predictY(LCGMs$LCGM3, newdata, var.time = "time", draws = TRUE, marg = TRUE)

#创建数据框
pred_dfci <- data.frame(
  time = plotpred[["times"]]$time,
  class1 = plotpred[["pred"]][, "Ypred_class1"],
  class2 = plotpred[["pred"]][, "Ypred_class2"],
  class3 = plotpred[["pred"]][, "Ypred_class3"],
  #添加置信区间数据
  class1_lower = plotpred[["pred"]][, "lower.Ypred_class1"],
  class1_upper = plotpred[["pred"]][, "upper.Ypred_class1"],
  class2_lower = plotpred[["pred"]][, "lower.Ypred_class2"],
  class2_upper = plotpred[["pred"]][, "upper.Ypred_class2"],
  class3_lower = plotpred[["pred"]][, "lower.Ypred_class3"],
  class3_upper = plotpred[["pred"]][, "upper.Ypred_class3"]
)

#直接绘制3条曲线和置信区间
LCGM_plotci <- ggplot(pred_dfci, aes(x = time)) +
  #绘制置信区间（ribbon）
  geom_ribbon(aes(ymin = class1_lower, ymax = class1_upper, fill = "Class 1"), alpha = 0.2) +
  geom_ribbon(aes(ymin = class2_lower, ymax = class2_upper, fill = "Class 2"), alpha = 0.2) +
  geom_ribbon(aes(ymin = class3_lower, ymax = class3_upper, fill = "Class 3"), alpha = 0.2) +
  #绘制预测线
  geom_line(aes(y = class1, color = "Class 1"), size = 0.5) +
  geom_line(aes(y = class2, color = "Class 2"), size = 0.5) +
  geom_line(aes(y = class3, color = "Class 3"), size = 0.5) +
  labs(
    x = "Years since baseline",
    y = expression(eGFR~"(ml/min/1.73"~m^2*")"),
    color = "Trajectory",
    fill = "Trajectory"
  ) +
  scale_color_manual(values = c(
    "Class 1" = "#9E9AC8",
    "Class 2" = "#FDAE6B",
    "Class 3" = "#6BAED6"
  ),
  labels = c(
    "Class 1" = "Low decline",
    "Class 2" = "Unstable",
    "Class 3" = "High-stable"
  )) +
  scale_fill_manual(values = c(
    "Class 1" = "#9E9AC8", 
    "Class 2" = "#FDAE6B",
    "Class 3" = "#6BAED6"),
    labels = c(
      "Class 1" = "Low decline",
      "Class 2" = "Unstable",
      "Class 3" = "High-stable"
    )) +
  scale_y_continuous(limits = c(70, 110)) +     #y轴范围
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = NA), #整体图像背景，边框无颜色
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.2),  #添加边框
    panel.grid.major = element_line(linewidth = 0.2), #调整主网格线粗细
    axis.ticks.x = element_line(color = "black"), #x轴刻度线
    axis.ticks.y = element_line(color = "black"), #y轴刻度线
    axis.ticks.length = unit(0.05, "cm"), #刻度线在外侧及长度
    axis.text = element_text(size = 5), #轴刻度字体大小
    axis.title = element_text(size = 6), #轴标题字体大小
    axis.ticks = element_line(size = 0.2), #轴刻度线粗细
    legend.key.height = unit(0.2, "cm"), #条形图例高度
    legend.key.width = unit(0.4, "cm"), #条形图例宽度
    legend.position = "top",
    legend.title = element_text(size = 6),
    legend.text = element_text(size = 5)
  )


#导出轨迹图
ggsave(
  filename = "LCGM_trajectory_plot.png",
  plot     = LCGM_plotci,
  width    = 7,
  height   = 7,
  units    = "cm", 
  dpi      = 600
)

###（4）轨迹分组
LCGM_group <- LCGMs$LCGM3$pprob[, c(1, 2)] #提取accountid和class列
names(LCGM_group)[names(LCGM_group) == "class"] <- "eGFR_group" #重命名

###（5）与其他数据合并
data_traj <- merge(final17_24, LCGM_group, by = "accountid")


#每个accountid的CMDs_event列有2值的，新变量CMDs编码为2，否则为1
data_traj2 <- data_traj %>%
  group_by(accountid) %>%
  mutate(
    CMDs = if_else(any(CMDs_event == 2, na.rm = TRUE), 2L, 1L),
    CMDs = factor(CMDs, levels = c(1, 2), labels = c("No", "Yes")) #转换为有标签因子型
  ) %>%
  arrange(exdate, .by_group = TRUE) %>% # 每个 accountid 内按日期排序（早→晚）
  dplyr::slice(1) %>% # 取最早那一行
  ungroup()

###----------其它预测因子编码----------
data_traj2 <- data_traj2 %>%
  mutate(
    across(c(5:48, 50, 53:56, 60:63, 89:117, 128:130), ~ as.numeric(.x)), #这些列从字符转为数值
    
    #教育水平Education: NA=(0), 1=Illiterate (scholar: 1), 2=Primary school (scholar: 2), 3=Middle school and above (scholar: 3-7)
    Education = case_when(
      Education == 0 ~ NA_real_,
      Education == 1 ~ 1,
      Education == 2 ~ 2,
      Education >= 3 ~ 3),
    Education = factor(
      Education,
      levels = c(1, 2, 3),
      labels = c("Illiterate", "Primary school", "Middle school and above")),
    
    #职业Occupation: NA=(0), 1=Non-agricultural work (occupation: 1-19,21-26), 2=Agricultural work (occupation: 20)
    Occupation = case_when(
      Occupation == 0  ~ NA_real_,
      Occupation == 20 ~ 2, 
      Occupation %in% c(1:19, 21:26) ~ 1),
    Occupation = factor(
      Occupation,
      levels = c(1, 2),
      labels = c("Non-agricultural work", "Agricultural work")),
    
    #婚姻状态Marital_status: NA=(0), 1=Other (marriage: 1,3,4), 2=Married (marriage: 2)
    Marital_status = case_when(
      Marital_status == 0 ~ NA_real_,
      Marital_status == 2 ~ 2, 
      Marital_status %in% c(1, 3, 4) ~ 1),
    Marital_status = factor(
      Marital_status,
      levels = c(1, 2),
      labels = c("Other", "Married")),
    
    #个人年收入Personal_annual_income: NA=(0), 1=Less than 100,000 yuan (yearincome:1-2)，2=100,000-200,000 yuan (yearincome:3-4)，3=More than 200,000 yuan (yearincome:5-6)
    Personal_annual_income = case_when(
      Personal_annual_income == 0 ~ NA_real_,
      Personal_annual_income %in% c(1, 2) ~ 1,
      Personal_annual_income %in% c(3, 4) ~ 2,
      Personal_annual_income %in% c(5, 6) ~ 3),
    Personal_annual_income = factor(
      Personal_annual_income,
      levels = c(1, 2, 3),
      labels = c(
        "Less than 100,000 yuan",
        "100,000-200,000 yuan",
        "More than 200,000 yuan")),
    
    #血型Blood_type: NA=(0/5), 1=A (BloodType:1), 2=B (BloodType:2), 3=O (BloodType:3), 4=AB (BloodType:4)
    Blood_type = case_when(
      Blood_type %in% c(0, 5) ~ NA_real_,
      TRUE ~ Blood_type),
    Blood_type = factor(
      Blood_type,
      levels = c(1, 2, 3, 4),
      labels = c("A", "B", "O", "AB")),
    
    #吸烟状况Smoking: NA=(0), 1=No (smokeornot: 1-2), 2=Yes (smokeornot: 3-5)
    Smoking = case_when(
      Smoking == 0 ~ NA_real_,
      Smoking %in% c(1, 2) ~ 1,
      Smoking %in% c(3, 4, 5) ~ 2),
    Smoking = factor(
      Smoking,
      levels = c(1, 2),
      labels = c("No", "Yes")),
    
    #饮酒状况Drinking: NA=(0), 1=No (drinkornot: 1-2), 2=Yes (drinkornot: 3-5)
    Drinking = case_when(
      Drinking == 0 ~ NA_real_,
      Drinking %in% c(1, 2) ~ 1,
      Drinking %in% c(3, 4, 5) ~ 2),
    Drinking = factor(
      Drinking,
      levels = c(1, 2),
      labels = c("No", "Yes")),
    
    #定时定量进食Regular_eating: NA=(0), 1=No (foodtime: 1), 2=Yes (foodtime: 2)
    Regular_eating = case_when(
      Regular_eating == 0 ~ NA_real_,
      TRUE ~ Regular_eating),
    Regular_eating = factor(
      Regular_eating,
      levels = c(1, 2),
      labels = c("No", "Yes")),
    
    #夜间睡眠Nighttime_sleep: 1= < 6h (sleeptime: 1-2), 2= 6-8h (sleeptime: 3-4), 3= > 8h (sleeptime: 5-6)
    Nighttime_sleep = case_when(
      Nighttime_sleep == 0 ~ NA_real_,
      Nighttime_sleep %in% c(1, 2) ~ 1,
      Nighttime_sleep %in% c(3, 4) ~ 2,
      Nighttime_sleep %in% c(5, 6) ~ 3),
    Nighttime_sleep = factor(
      Nighttime_sleep,
      levels = c(1, 2, 3),
      labels = c("<6 h", "6-8 h", ">8 h")),
    
    #运动频率Exercise_frequency: NA=(0), 1=No exercise or very rarely (FirstSportFrequ：5), 2=Once per week (FirstSportFrequ：4), 3=2–3 times per week (FirstSportFrequ：3), 4=Once or more times per day (FirstSportFrequ：1-2)
    Exercise_frequency = case_when(
      Exercise_frequency == 0       ~ NA_real_,
      Exercise_frequency == 5       ~ 1,
      Exercise_frequency == 4       ~ 2,
      Exercise_frequency == 3       ~ 3,
      Exercise_frequency %in% c(1,2)~ 4),
    Exercise_frequency = factor(
      Exercise_frequency,
      levels = c(1, 2, 3, 4),
      labels = c(
        "No exercise or very rarely", 
        "Once per week", 
        "2-3 times per week", 
        "Once or more times per day")),
    
    #自评的健康状况Self_rated_health: NA=(0), 1=Poor(SameAgeHealth: 4+5), 2=Fair(SameAgeHealth: 3), 3=Good(SameAgeHealth: 1+2)
    Self_rated_health = case_when(
      Self_rated_health == 0 ~ NA_real_,
      Self_rated_health %in% c(4, 5) ~ 1,
      Self_rated_health == 3 ~ 2,
      Self_rated_health %in% c(1, 2) ~ 3),
    Self_rated_health = factor(
      Self_rated_health,
      levels = c(1, 2, 3),
      labels = c("Poor", "Fair", "Good")),
    
    #疾病分类转换Arthritis, Asthma, nopsick, psick01:psick26: 1=No(F/FALSE), 2=Yes(T/TRUE)
    across(c(Arthritis, Asthma, nopsick, psick01:psick26),
           ~ factor(case_when(
             . %in% c("T", "TRUE", "2")  ~ 2,
             . %in% c("F", "FALSE", "1") ~ 1,
             TRUE ~ NA_real_),
             levels = c(1, 2),
             labels = c("No", "Yes")
           )
    ) 
  )

#选择癌症变量
Cancer_vars <- c("psick01","psick02","psick03","psick04",
                 "psick05","psick06","psick07","psick08","psick25")

data_traj2 <- data_traj2 %>%
  rowwise() %>%
  mutate(
    #癌症Cancer: 1=No(psick01-08、psick25:全为1或nopsick == 2), 2=Yes(psick01-08、psick25:任一为2)
    Cancer = case_when(
      any(c_across(all_of(Cancer_vars)) == "Yes") ~ 2,
      all(c_across(all_of(Cancer_vars)) == "No") | nopsick == "Yes" ~ 1,
      TRUE ~ NA_real_),
    Cancer = factor(
      Cancer,
      levels = c(1, 2),
      labels = c("No", "Yes")),
    
    #肝病Liver_diseases: 1=No(psick17-18:全为1或nopsick == 2), 2=Yes(psick17-18任一为2)
    Liver_diseases = case_when(
      any(c_across(all_of(c("Hepatitis", "Cirrhosis"))) == "Yes") ~ 2,
      all(c_across(all_of(c("Hepatitis", "Cirrhosis"))) == "No") | nopsick == "Yes" ~ 1,
      TRUE ~ NA_real_),
    Liver_diseases = factor(
      Liver_diseases,
      levels = c(1, 2),
      labels = c("No", "Yes"))
  ) %>%
  ungroup()

data_traj2 <- data_traj2 %>%
  mutate(
    #以下变量<0转换为NA
    across(
      c(
        ESLLS, ESRLS, LHearing, RHearing, 
        SBP, DBP, Height, Weight, BMI,
        Waist_circumference, Hip_circumference, BFP,
        Urine_protein, Urine_glucose, Insulin, SpO2,
        HS_CRP, HbA1c, TC, HDL_C, LDL_C, TG,
        BUN, FBG, UA, WBC, HB, MCV, PLT, HCT
      ),
      ~ if_else(. < 0, NA_real_, .)),
    
    #视力Vision：根据ESLLS、ESRLS取最大值得到新变量Vision 
    Vision = pmax(ESLLS, ESRLS, na.rm = TRUE),
    
    #听力Hearing：根据LHearing、RHearing取最大值得到新变量Hearing
    Hearing = pmax(LHearing, RHearing, na.rm = TRUE),
    
    # 重新计算 BMI（kg / m^2）
    BMI = if_else(
      !is.na(Height) & !is.na(Weight) & Height > 0 & Weight > 0,
      Weight / (Height / 100)^2,
      NA_real_
    ),
    #BMI分类
    BMI = case_when(
      BMI < 18.5 ~ 1,
      BMI >= 18.5 & BMI < 24 ~ 2,
      BMI >= 24 ~ 3,
      TRUE ~ NA_real_
    ),
    BMI = factor(
      BMI,
      levels = c(1, 2, 3),
      labels = c("<18.5", "18.5-24", "≥24")),
    
    #尿蛋白分类
    Urine_protein = case_when(
      Urine_protein == 1        ~ 1,
      Urine_protein == 2        ~ 2,
      Urine_protein %in% 3:5    ~ 3,
      TRUE                    ~ NA_real_
    ),
    Urine_protein = factor(
      Urine_protein,
      levels = c(1, 2, 3),
      labels = c("Negative", "Weakly positive", "Positive")),
    
    #尿糖分类
    Urine_glucose = case_when(
      Urine_glucose == 1        ~ 1,
      Urine_glucose == 2        ~ 2,
      Urine_glucose %in% 3:5    ~ 3,
      TRUE            ~ NA_real_
    ),
    Urine_glucose = factor(
      Urine_glucose,
      levels = c(1, 2, 3),
      labels = c("Negative", "Weakly positive", "Positive")),
    
    SpO2 = case_when(
      SpO2 > 100 | SpO2 < 70 ~ NA_real_,
      TRUE ~ SpO2
    )
  )

#提取需要的变量
vars_needed <- c(
  "Age","Sex","Education","Marital_status","Smoking","Drinking",
  "Cancer","Arthritis","Asthma","Thyroid_diseases","Tuberculosis",
  "Digestive_diseases","Liver_diseases","Urolithiasis","Gout","Anemia",
  "Vision","Hearing","SBP","DBP","BMI","Waist_circumference",
  "Hip_circumference","BFP","Urine_protein","Urine_glucose",
  "HbA1c","TC","HDL_C","LDL_C","TG","BUN","FBG","UA","WBC",
  "HB","MCV","PLT","HCT","eGFR_group", "CMDs"
)
#最终分析数据集
data_analyze <- data_traj2 %>%
  dplyr::select(all_of(vars_needed))

#复制数据集，用于替换变量名
data_analyze1 <- data_analyze
idx2 <- match(names(data_analyze1), variable_dict$analysis_name)
colnames(data_analyze1)[!is.na(idx2)] <- variable_dict$display_name[idx2[!is.na(idx2)]]


###-----------多重共线性诊断-----------
#删除eGFR_group和CMDs
data_corr <- data_analyze1 %>%
  select(-eGFR_group, -CMDs)

#所有因子变量均转换为数值型
data_corr_num <- data_corr %>% 
  mutate(across(where(is.factor), ~ as.numeric(.)))

#执行相关性检验
corr_matrix <- corr.test(data_corr_num, method = "spearman")
#提取相关性系数
r <- corr_matrix$r

#绘图
png("C:/Users/74077/Desktop/corrplot.png", width = 3500, height = 3000, res = 600)
corrplot.mixed(r,
               lower = "number", #下半部图形类型为数字
               number.cex = 1.5, #相关性系数字体大小
               upper = "color", #上半部为色块
               lower.col = "black", #下半部颜色
               tl.pos = "lt", #标签显示在左侧
               tl.col = "black", #标签颜色为黑色
               tl.srt = 30, #上方标签显示角度
               tl.cex = 2.5, #调整文字标签大小
               diag = "l", #l表明图形交界处显示下半部分的内容
               cl.cex = 2.5, #图例文字大小
               cl.ratio = 0.1 #图例宽度比例
) 
dev.off()

#找出相关系数绝对值 > 0.9 的变量对
r_id <- which(abs(r) > 0.9 & abs(r) < 1, arr.ind = TRUE)
high_corr <- data.frame(
  var1 = rownames(r)[r_id[,1]],
  var2 = colnames(r)[r_id[,2]],
  cor  = r[r_id]
)
#去重（只保留下三角）
high_corr <- high_corr[r_id[,1] < r_id[,2], ]
#识别删除的第一个变量名
corr_vars_drop <- unique(high_corr$var1)



###-----------缺失值情况-----------
#计算缺失数和缺失比例
missing <- data.frame(
  #添加变量名
  Variable = names(data_corr), 
  #缺失数
  Missing_Count <- sapply(
    data_corr,
    function(x) formatC(sum(is.na(x)), format = "d", big.mark = ",")
  ),  #缺失比例，保留两位小数
  Missing_Percentage = sapply(
    data_corr, 
    function(x) round(sum(is.na(x)) / nrow(data_corr) * 100, 2))   
)
#提取data_corr中缺失值超过20%的变量名
missing_variable <- missing$Variable[missing$Missing_Percentage > 20]

#删除缺失值超过20%的变量和相关性强的变量
#找到这些变量名在data_analyze1中的列索引
missing_columns <- which(names(data_analyze1) %in% c(missing_variable, corr_vars_drop))
#删除这些列
data_analyze2 <- data_analyze[, -missing_columns]

##缺失值情况整理成表格
#判断变量类型并添加类型列
missing$Type <- sapply(data_corr, function(x) if (is.factor(x)) "Categorical" else "Continuous")
#对于分类变量，输出“编码-标签”形式的描述
missing$Description <- sapply(names(data_corr), function(x) {
  if (is.factor(data_corr[[x]])) {
    levels_x <- levels(data_corr[[x]]) #获取变量x的所有水平并存储在 levels_x 中
    paste(paste(1:length(levels_x), levels_x, sep = ": "), collapse = ", ") #变量编码：变量标签
  } 
  #数值型变量单位描述
  else if (x == "Age") {"years"} 
  else if (x == "Hearing") {"dB"} 
  else if (x == "SBP" | x == "DBP") {"mmHg"} 
  else if (x == "Waist circumference" | x == "Hip circumference") {"cm"} 
  else if (x == "BFP" | x == "SpO2" | x == "HbA1c" | x == "HCT") {"%"} 
  else if (x == "Insulin") {"uU/ml"} 
  else if (x == "HS-CRP") {"mg/L"} 
  else if (x == "TC" | x == "HDL-C" | x == "LDL-C" | x == "TG" | x == "BUN" | x == "FBG") {"mmol/L"} 
  else if (x == "UA") {"μmol/L"} 
  else if (x == "WBC" | x == "PLT") {"×10^9/L"} 
  else if (x == "HB") {"g/L"} 
  else if (x == "MCV") {"fL"} 
  else { "-" } 
})
#合并缺失数和缺失比例
missing$"Missing data (n=10,967) [n (%)]" <- paste(missing$Missing_Count, " (", missing$Missing_Percentage, ")", sep = "")
#选择最终需要的列
missing_description <- missing[, -c(2:3)]
write.csv(missing_description, "missing_description.csv", row.names = FALSE)


###-----------基线特征-----------
# 数据字典单位
data_dict <- data.frame(rbind(
  c("Age", "Age (years), mean ± SD"),
  c("Sex", "Sex, n (%)"),
  c("Education", "Education, n (%)"),
  c("Marital_status", "Marital status, n (%)"),
  c("Smoking", "Smoking, n (%)"),
  c("Drinking", "Drinking, n (%)"),
  c("Cancer", "Cancer, n (%)"),
  c("Arthritis", "Arthritis, n (%)"),
  c("Asthma", "Asthma, n (%)"),
  c("Thyroid_diseases", "Thyroid diseases, n (%)"),
  c("Tuberculosis", "Tuberculosis, n (%)"),
  c("Digestive_diseases", "Digestive diseases, n (%)"),
  c("Liver_diseases", "Liver diseases, n (%)"),
  c("Urolithiasis", "Urolithiasis, n (%)"),
  c("Gout", "Gout, n (%)"),
  c("Anemia", "Anemia, n (%)"),
  c("Vision", "Vision, mean ± SD"),
  c("Hearing", "Hearing (dB), mean ± SD"),
  c("SBP", "SBP (mmHg), mean ± SD"),
  c("DBP", "DBP (mmHg), mean ± SD"),
  c("BMI", "BMI, n (%)"),
  c("Waist_circumference", "Waist circumference (cm), mean ± SD"),
  c("Hip_circumference", "Hip circumference (cm), mean ± SD"),
  c("BFP", "BFP (%), mean ± SD"),
  c("Urine_protein", "Urine protein, n (%)"),
  c("Urine_glucose", "Urine glucose, n (%)"),
  c("HbA1c", "HbA1c (%), mean ± SD"),
  c("TC", "TC (mmol/L), mean ± SD"),
  c("HDL_C", "HDL-C (mmol/L), mean ± SD"),
  c("LDL_C", "LDL-C (mmol/L), mean ± SD"),
  c("TG", "TG (mmol/L), mean ± SD"),
  c("BUN", "BUN (mmol/L), mean ± SD"),
  c("FBG", "FBG (mmol/L), mean ± SD"),
  c("UA", "UA (μmol/L), mean ± SD"),
  c("WBC", "WBC (×10^9/L), mean ± SD"),
  c("HB", "HB (g/L), mean ± SD"),
  c("MCV", "MCV (fL), mean ± SD"),
  c("PLT", "PLT (×10^9/L), mean ± SD"),
  c("HCT", "HCT (%), mean ± SD"),
  c("CMDs", "CMDs")
))
names(data_dict) <- c("short", "long")

#删除eGFR_group
data_baseline <- data_analyze %>%
  select(-eGFR_group)
#变量名转换
colnames(data_baseline) <- data_dict$long[match(names(data_baseline), data_dict$short)]

#基线特征比较
baseline <- data_baseline %>% 
  tbl_summary(by = "CMDs", #根据RDKF进行分组统计
              statistic = list(all_continuous() ~ "{mean} ± {sd}", #连续变量统计：均值±标准差
                               all_categorical() ~ "{n} ({p})"), #分类变量统计：频数（百分比）
              digits = list(all_continuous() ~ 2, #连续变量均和标准差保留2位小数
                            all_categorical() ~ c(0, 2)), #分类变量的百分比保留2位小数
              type = list(all_continuous() ~ "continuous", #连续变量显示：均值±标准差
                          all_categorical() ~ "categorical"), #分类变量显示所有类别：频数（百分比）
              missing = "no" #不显示缺失值的行
  ) %>% 
  add_p(
    all_continuous() ~ "t.test",# 分类变量自动选择卡方检验或Fisher精确检验    
    pvalue_fun = ~ style_pvalue(.x, digits = 3)) %>% #添加P值并保留3位小数 
  add_overall() %>% #添加一列汇总整体统计量
  modify_header(statistic ~ "t test/χ^2 (df)", p.value = "P value") %>% 
  modify_table_styling(
    columns = statistic,
    fmt_fun = function(x) style_number(x, digits = 3)
  ) %>%
  modify_table_styling(
    columns = parameter,
    fmt_fun = function(x) style_number(x, digits = 0)
  ) %>%
  modify_column_merge(
    pattern = "{statistic} ({parameter})",  # 格式：统计量(自由度)
    rows = !is.na(statistic)
  )

#导出为word
baseline %>%
  as_flex_table() %>%
  flextable::save_as_docx(baseline, path = "C:/Users/74077/Desktop/baseline characteristics.docx")

write.csv(tb, "baseline_characteristics.csv", row.names = FALSE, fileEncoding = "UTF-8-BOM")
# install.packages("readr")
outfile <- file.path(getwd(), "baseline_characteristics.csv")
readr::write_excel_csv(tb, outfile)


###-----------缺失值处理：随机森林插补-----------
#转换为数据框
data_analyze2 <- as.data.frame(data_analyze2)
#设置随机种子，确保插补可重复性
set.seed(2025)
#使用missForest()对data_analyze2插补缺失值
data_missresult <- missForest(data_analyze2)
#提取插补后的数据并转换为数据框
data_analyze3 <- as.data.frame(data_missresult$ximp)


###-----------划分数据集-----------
##轨迹组划分
#Low decline和High-stable组
data_LH <- data_analyze3 %>%
  filter(eGFR_group %in% c(1, 3)) %>%
  select(-eGFR_group)  # 删除 eGFR_group 列

#Unstable和High-stable组
data_UH <- data_analyze3 %>%
  filter(eGFR_group %in% c(2, 3)) %>%
  select(-eGFR_group)  # 删除 eGFR_group 列

##Low decline和High-stable组划分训练队列与测试队列
#设置随机种子，确保划分可重复性
set.seed(1217)
#createDataPartition()会自动从y的各个level随机取出等比例的数据来，p表示训练队列与测试队列占比，list = F表示返回的结果是一个向量，times = 1表示分割操作只执行一次 
trainindex_LH <- createDataPartition(data_LH$CMDs, p = 0.7, list = F, times = 1)
#划分出训练队列
data_LH_train <- data_LH[trainindex_LH, ]
#划分出测试队列
data_LH_test <- data_LH[-trainindex_LH, ]

##Unstable和High-stable组划分训练队列与测试队列
#设置随机种子，确保划分可重复性
set.seed(1217)
#createDataPartition()会自动从y的各个level随机取出等比例的数据来，p表示训练队列与测试队列占比，list = F表示返回的结果是一个向量，times = 1表示分割操作只执行一次 
trainindex_UH <- createDataPartition(data_UH$CMDs, p = 0.7, list = F, times = 1)
#划分出训练队列
data_UH_train <- data_UH[trainindex_UH, ]
#划分出测试队列
data_UH_test <- data_UH[-trainindex_UH, ]


###-----------变量标准化-----------
##Low decline和High-stable组变量标准化
#使用preProcess()对训练队列进行标准化处理（中心化和缩放），使得每列数据的均值为0，标准差为1
standardized_para_LH <- preProcess(data_LH_train, method = c("center", "scale"))
#对训练队列进行标准化
data_LH_train_std <- predict(standardized_para_LH, newdata = data_LH_train)
#对测试队列进行标准化，使用训练队列的标准化参数
data_LH_test_std <- predict(standardized_para_LH, newdata = data_LH_test)

##Unstable和High-stable组变量标准化
#使用preProcess()对训练队列进行标准化处理（中心化和缩放），使得每列数据的均值为0，标准差为1
standardized_para_UH <- preProcess(data_UH_train, method = c("center", "scale"))
#对训练队列进行标准化
data_UH_train_std <- predict(standardized_para_UH, newdata = data_UH_train)
#对测试队列进行标准化，使用训练队列的标准化参数
data_UH_test_std <- predict(standardized_para_UH, newdata = data_UH_test)


##XGBoost、SVM不能处理因子变量，数据集转换为数值型，除了CMDs
#定义函数
to_num_except_CMDs <- function(df) {
  df %>%
    mutate(
      across(
        where(is.factor) & -CMDs,
        ~ as.numeric(.)
      )
    )
}

#执行转换
data_LH_train_std_num <- to_num_except_CMDs(data_LH_train_std)
data_LH_test_std_num  <- to_num_except_CMDs(data_LH_test_std)
data_UH_train_std_num  <- to_num_except_CMDs(data_UH_train_std)
data_UH_test_std_num  <- to_num_except_CMDs(data_UH_test_std)
LH_train_smotenc_num  <- to_num_except_CMDs(LH_train_smotenc)
UH_train_smotenc_num  <- to_num_except_CMDs(UH_train_smotenc)


###-----------构建预测模型: mlr3-----------
#忽略训练过程中控制台输出的信息，'warn'（只输出警告和错误信息）
lgr::get_logger("mlr3")$set_threshold("warn")
lgr::get_logger("bbotk")$set_threshold("warn")

##-----构建预测模型: Low decline和High-stable组-----
#创建训练队列任务
LH_train_task <- as_task_classif(data_LH_train_std_num, target = "CMDs")
#创建测试队列任务
LH_test_task <- as_task_classif(data_LH_test_std_num, target = "CMDs")


##逻辑回归LR
#选择算法，预测输出为概率
LH_lr_learner <- lrn("classif.log_reg", predict_type = "prob")
#对训练队列进行训练
LH_lr_learner$train(LH_train_task)
#对训练队列进行预测
LH_train_lr_pred <- LH_lr_learner$predict(LH_train_task)
#对测试队列进行预测
LH_test_lr_pred <- LH_lr_learner$predict(LH_test_task)
LH_test_lr_pred$score(msr("classif.auc"))


##随机森林RF
#选择算法，预测输出为概率
LH_rf_learner <- lrn("classif.ranger", predict_type = "prob")   
#选择调整的超参数及范围
LH_rf_learner$param_set$values <- list(
  num.trees = to_tune(10, 100), #树的数量：控制森林中树的数量，影响模型稳定性和计算效率。
  max.depth = to_tune(1, 5), #树的最大深度：限制树的最大深度，防止过拟合。
  min.node.size = to_tune(p_int(5, 10)), #内部节点最小样本数：设置节点最小样本数，控制树的分裂程度。p_int() 是定义整数参数的范围，适用于新版 mlr3tuning
  mtry = to_tune(5, 10), #分裂时的随机特征数：控制分裂时的随机特征数，平衡多样性与准确性。
  sample.fraction = to_tune(0.5, 1) #随机抽取的样本比例
)
#设置随机种子，确保调参可重复性
set.seed(123)
#执行调参
LH_rf <- tune(tuner = tnr("grid_search", resolution = 5), #网格搜索；resolution = 5表示每个要调优的参数在其范围内划分为5个等间隔的值
              task = LH_train_task, #指定调优任务
              learner = LH_rf_learner, #指定要调优的学习器
              resampling = rsmp("cv", folds = 5), #5折交叉验证
              measure = msr("classif.auc") #模型评估指标AUC
)
#最好的超参数应用到算法上
LH_rf_learner$param_set$values <- LH_rf$result_learner_param_vals
#对训练队列进行训练
LH_rf_learner$train(LH_train_task)
#对训练队列进行预测
LH_train_rf_pred <- LH_rf_learner$predict(LH_train_task)
#对测试队列进行预测
LH_test_rf_pred <- LH_rf_learner$predict(LH_test_task)


##XGBoost
LH_xgb_learner <- lrn("classif.xgboost", predict_type = "prob")
LH_xgb_learner$param_set$values <- list(
  nrounds = to_tune(500, 1000), #树的数量：控制迭代次数，决定模型学习的程度。
  max_depth = to_tune(1, 10), #树的最大深度：控制树的复杂度，防止过拟合。
  eta = to_tune(0.001, 0.05), #学习率：控制学习的速度，影响收敛和稳定性。
  min_child_weight = to_tune(1, 10), #每个叶节点最小实例权重：限制分裂，防止过拟合。
  subsample = to_tune(0.1, 0.5) #子样本比例：提供随机性，减少过拟合风险，增加多样性。
)
set.seed(123)
LH_xgb <- tune(tuner = tnr("grid_search", resolution = 5),
               task = LH_train_task,
               learner = LH_xgb_learner,
               resampling = rsmp("cv", folds = 5),
               measure = msr("classif.auc")
)
LH_xgb_learner$param_set$values <- LH_xgb$result_learner_param_vals
LH_xgb_learner$train(LH_train_task)
LH_train_xgb_pred <- LH_xgb_learner$predict(LH_train_task)
LH_test_xgb_pred <- LH_xgb_learner$predict(LH_test_task)


##支持向量机SVM
LH_svm_learner <- lrn("classif.svm", predict_type = "prob")
LH_svm_learner$param_set$values <- list(
  type = "C-classification", #分类问题
  kernel = "radial", #径向基函数（Radial Basis Function, RBF）核：处理复杂的非线性分类问题
  cost = to_tune(0.001, 1), #C或惩罚参数：对错误分类进行惩罚的程度
  gamma = to_tune(0.001, 1) #gamma参数：定义了RBF核的宽度，影响着数据点映射到高维特征空间后的分布
)
set.seed(123)
LH_svm <- tune(tuner = tnr("grid_search", resolution = 5), 
               task = LH_train_task, 
               learner = LH_svm_learner, 
               resampling = rsmp("cv", folds = 5),
               measure = msr("classif.auc")
)
LH_svm_learner$param_set$values <- LH_svm$result_learner_param_vals
LH_svm_learner$train(LH_train_task)
LH_train_svm_pred <- LH_svm_learner$predict(LH_train_task)
LH_test_svm_pred <- LH_svm_learner$predict(LH_test_task)


##K近邻KNN
LH_knn_learner <- lrn("classif.kknn", predict_type = "prob")
LH_knn_learner$param_set$values <- list(
  k = to_tune(500, 1000) #最近邻的个数
)
set.seed(123)
LH_knn <- tune(tuner = tnr("grid_search", resolution = 5), 
               task = LH_train_task, 
               learner = LH_knn_learner, 
               resampling = rsmp("cv", folds = 5),
               measure = msr("classif.auc")
)
LH_knn_learner$param_set$values <- LH_knn$result_learner_param_vals
LH_knn_learner$train(LH_train_task)
LH_train_knn_pred <- LH_knn_learner$predict(LH_train_task)
LH_test_knn_pred <- LH_knn_learner$predict(LH_test_task)


##决策树DT
LH_dt_learner <- lrn("classif.rpart", predict_type = "prob")
LH_dt_learner$param_set$values <- list(
  minsplit = to_tune(20, 50), #最小分割数
  minbucket = to_tune(10, 20), #最小叶节点中观测值的数量
  cp = to_tune(0.01, 0.1), #复杂度参数
  maxdepth = to_tune(3, 10) #树的最大深度
)
set.seed(123)
LH_dt <- tune(tuner = tnr("grid_search", resolution = 5), 
              task = LH_train_task, 
              learner = LH_dt_learner, 
              resampling = rsmp("cv", folds = 5),
              measure = msr("classif.auc")
)
LH_dt_learner$param_set$values <- LH_dt$result_learner_param_vals
LH_dt_learner$train(LH_train_task)
LH_train_dt_pred <- LH_dt_learner$predict(LH_train_task)
LH_test_dt_pred <- LH_dt_learner$predict(LH_test_task)


##人工神经网络ANN
LH_ann_learner <- lrn("classif.nnet", predict_type = "prob")
LH_ann_learner$param_set$values <- list(
  size = to_tune(3, 10), #隐藏层神经元的个数,size 越大,模型容量越强、能拟合更复杂的非线性关系
  decay = to_tune(0.001, 0.01) #权重衰减系数/ L2 正则化强度,decay 越大 → 对权重惩罚越强，权重会被压小 → 更不容易过拟合，但过大时会严重欠拟合
)
set.seed(123)
LH_ann <- tune(tuner = tnr("grid_search", resolution = 5), 
               task = LH_train_task, 
               learner = LH_ann_learner, 
               resampling = rsmp("cv", folds = 5),
               measure = msr("classif.auc")
)
LH_ann_learner$param_set$values <- LH_ann$result_learner_param_vals
LH_ann_learner$train(LH_train_task)
LH_train_ann_pred <- LH_ann_learner$predict(LH_train_task)
LH_test_ann_pred <- LH_ann_learner$predict(LH_test_task)


##-----构建预测模型: Unstable和High-stable组-----
#创建训练队列任务
UH_train_task <- as_task_classif(data_UH_train_std_num, target = "CMDs")
#创建测试队列任务
UH_test_task <- as_task_classif(data_UH_test_std_num, target = "CMDs")


##逻辑回归LR
#选择算法，预测输出为概率
UH_lr_learner <- lrn("classif.log_reg", predict_type = "prob")
#对训练队列进行训练
UH_lr_learner$train(UH_train_task)
#对训练队列进行预测
UH_train_lr_pred <- UH_lr_learner$predict(UH_train_task)
#对测试队列进行预测
UH_test_lr_pred <- UH_lr_learner$predict(UH_test_task)

##随机森林RF
#选择算法，预测输出为概率
UH_rf_learner <- lrn("classif.ranger", predict_type = "prob")   
#选择调整的超参数及范围
UH_rf_learner$param_set$values <- list(
  num.trees = to_tune(10, 100), #树的数量：控制森林中树的数量，影响模型稳定性和计算效率。
  max.depth = to_tune(1, 5), #树的最大深度：限制树的最大深度，防止过拟合。
  min.node.size = to_tune(p_int(1, 5)), #内部节点最小样本数：设置节点最小样本数，控制树的分裂程度。p_int() 是定义整数参数的范围，适用于新版 mlr3tuning
  mtry = to_tune(1, 5), #分裂时的随机特征数：控制分裂时的随机特征数，平衡多样性与准确性。
  sample.fraction = to_tune(0.3, 1) #随机抽取的样本比例
)
#设置随机种子，确保调参可重复性
set.seed(123)
#执行调参
UH_rf <- tune(tuner = tnr("grid_search", resolution = 5), #网格搜索；resolution = 5表示每个要调优的参数在其范围内划分为5个等间隔的值
              task = UH_train_task, #指定调优任务
              learner = UH_rf_learner, #指定要调优的学习器
              resampling = rsmp("cv", folds = 5), #5折交叉验证
              measure = msr("classif.auc") #模型评估指标AUC
)
#最好的超参数应用到算法上
UH_rf_learner$param_set$values <- UH_rf$result_learner_param_vals
#对训练队列进行训练
UH_rf_learner$train(UH_train_task)
#对训练队列进行预测
UH_train_rf_pred <- UH_rf_learner$predict(UH_train_task)
#对测试队列进行预测
UH_test_rf_pred <- UH_rf_learner$predict(UH_test_task)


##XGBoost
UH_xgb_learner <- lrn("classif.xgboost", predict_type = "prob")
UH_xgb_learner$param_set$values <- list(
  nrounds = to_tune(100, 500), #树的数量：控制迭代次数，决定模型学习的程度。
  max_depth = to_tune(1, 10), #树的最大深度：控制树的复杂度，防止过拟合。
  eta = to_tune(0.001, 0.05), #学习率：控制学习的速度，影响收敛和稳定性。
  min_child_weight = to_tune(1, 10), #每个叶节点最小实例权重：限制分裂，防止过拟合。
  subsample = to_tune(0.5, 1) #子样本比例：提供随机性，减少过拟合风险，增加多样性。
)
set.seed(123)
UH_xgb <- tune(tuner = tnr("grid_search", resolution = 5),
               task = UH_train_task,
               learner = UH_xgb_learner,
               resampling = rsmp("cv", folds = 5),
               measure = msr("classif.auc")
)
UH_xgb_learner$param_set$values <- UH_xgb$result_learner_param_vals
UH_xgb_learner$train(UH_train_task)
UH_train_xgb_pred <- UH_xgb_learner$predict(UH_train_task)
UH_test_xgb_pred <- UH_xgb_learner$predict(UH_test_task)


##支持向量机SVM
UH_svm_learner <- lrn("classif.svm", predict_type = "prob")
UH_svm_learner$param_set$values <- list(
  type = "C-classification", #分类问题
  kernel = "radial", #径向基函数（Radial Basis Function, RBF）核：处理复杂的非线性分类问题
  cost = to_tune(1, 10), #C或惩罚参数：对错误分类进行惩罚的程度
  gamma = to_tune(0.001, 1) #gamma参数：定义了RBF核的宽度，影响着数据点映射到高维特征空间后的分布
)
set.seed(123)
UH_svm <- tune(tuner = tnr("grid_search", resolution = 5), 
               task = UH_train_task, 
               learner = UH_svm_learner, 
               resampling = rsmp("cv", folds = 5),
               measure = msr("classif.auc")
)
UH_svm_learner$param_set$values <- UH_svm$result_learner_param_vals
UH_svm_learner$train(UH_train_task)
UH_train_svm_pred <- UH_svm_learner$predict(UH_train_task)
UH_test_svm_pred <- UH_svm_learner$predict(UH_test_task)


##K近邻KNN
UH_knn_learner <- lrn("classif.kknn", predict_type = "prob")
UH_knn_learner$param_set$values <- list(
  k = to_tune(100, 1000) #最近邻的个数
)
set.seed(123)
UH_knn <- tune(tuner = tnr("grid_search", resolution = 5), 
               task = UH_train_task, 
               learner = UH_knn_learner, 
               resampling = rsmp("cv", folds = 5),
               measure = msr("classif.auc")
)
UH_knn_learner$param_set$values <- UH_knn$result_learner_param_vals
UH_knn_learner$train(UH_train_task)
UH_train_knn_pred <- UH_knn_learner$predict(UH_train_task)
UH_test_knn_pred <- UH_knn_learner$predict(UH_test_task)


##决策树DT
UH_dt_learner <- lrn("classif.rpart", predict_type = "prob")
UH_dt_learner$param_set$values <- list(
  minsplit = to_tune(20, 50), #最小分割数
  minbucket = to_tune(10, 20), #最小叶节点中观测值的数量
  cp = to_tune(0.01, 0.1), #复杂度参数
  maxdepth = to_tune(3, 10) #树的最大深度
)
set.seed(123)
UH_dt <- tune(tuner = tnr("grid_search", resolution = 5), 
              task = UH_train_task, 
              learner = UH_dt_learner, 
              resampling = rsmp("cv", folds = 5),
              measure = msr("classif.auc")
)
UH_dt_learner$param_set$values <- UH_dt$result_learner_param_vals
UH_dt_learner$train(UH_train_task)
UH_train_dt_pred <- UH_dt_learner$predict(UH_train_task)
UH_test_dt_pred <- UH_dt_learner$predict(UH_test_task)


##人工神经网络ANN
UH_ann_learner <- lrn("classif.nnet", predict_type = "prob")
UH_ann_learner$param_set$values <- list(
  size = to_tune(1, 10), #隐藏层神经元的个数,size 越大,模型容量越强、能拟合更复杂的非线性关系
  decay = to_tune(0.001, 0.01) #权重衰减系数/ L2 正则化强度,decay 越大 → 对权重惩罚越强，权重会被压小 → 更不容易过拟合，但过大时会严重欠拟合
)
set.seed(123)
UH_ann <- tune(tuner = tnr("grid_search", resolution = 5), 
               task = UH_train_task, 
               learner = UH_ann_learner, 
               resampling = rsmp("cv", folds = 5),
               measure = msr("classif.auc")
)
UH_ann_learner$param_set$values <- UH_ann$result_learner_param_vals
UH_ann_learner$train(UH_train_task)
UH_train_ann_pred <- UH_ann_learner$predict(UH_train_task)
UH_test_ann_pred <- UH_ann_learner$predict(UH_test_task)


###-----------模型性能评估：35个因子-----------
##-----模型性能评估：Low decline和High-stable组-----
#创建训练队列和测试队列的模型预测结果列表
LH_train_preds <- list(
  LR = LH_train_lr_pred, 
  RF = LH_train_rf_pred, 
  XGBoost = LH_train_xgb_pred, 
  SVM = LH_train_svm_pred, 
  KNN = LH_train_knn_pred, 
  DT = LH_train_dt_pred, 
  ANN = LH_train_ann_pred
)
LH_test_preds <- list(
  LR = LH_test_lr_pred, 
  RF = LH_test_rf_pred, 
  XGBoost = LH_test_xgb_pred, 
  SVM = LH_test_svm_pred, 
  KNN = LH_test_knn_pred, 
  DT = LH_test_dt_pred, 
  ANN = LH_test_ann_pred
)

#创建空的数据框，分别用于存储训练队列和测试队列的性能评估结果
LH_train_metrics <- data.frame()
LH_test_metrics <- data.frame()

#创建空的数据框，分别用于存储绘制训练队列和测试队列的ROC数据
LH_train_roc_data <- data.frame()
LH_test_roc_data <- data.frame()

#创建列表来存储训练队列和测试队列的ROC()结果，后面进行AUC的DeLong检验
LH_train_roc_auc <- list()
LH_test_roc_auc <- list()

#创建列表来存储训练队列和测试队列模型的预测概率数据（校准曲线）
LH_train_prob_data <- list()
LH_test_prob_data <- list()

#创建空的数据框，分别用于存储训练队列和测试队列的dca数据
LH_train_dca_data <- data.frame()
LH_test_dca_data <- data.frame()

##定义性能指标函数
calculate_metrics <- function(pred_prob, true_class, threshold) {
  #根据阈值生成预测类别
  pred_class <- ifelse(pred_prob >= threshold, "1", "0") 
  #构建混淆矩阵，确保所有类别都存在
  cm <- table(
    factor(pred_class, levels = c("0", "1")), #确保预测类别包含No和Yes
    factor(true_class, levels = c("0", "1")) #确保实际类别包含类别No和Yes
  )
  #提取混淆矩阵元素，防止缺少某些类别时报错
  TP <- cm["1", "1"] #真阳性
  TN <- cm["0", "0"] #真阴性
  FP <- cm["1", "0"] #假阳性
  FN <- cm["0", "1"] #假阴性
  #计算各项指标
  sensitivity <- TP / (TP + FN) #灵敏度Recall（召回率）
  specificity <- TN / (TN + FP) #特异度
  PPV <- TP / (TP + FP) #阳性预测值/精确率（Precision）
  NPV <- TN / (TN + FN) #阴性预测值
  CCR <- (TP + TN) / sum(cm) #准确率
  f1_score <- 2 * (PPV * sensitivity) / (PPV + sensitivity) #F1分数
  #返回结果
  list(Sensitivity = sensitivity, Specificity = specificity, PPV = PPV, NPV = NPV, CCR = CCR, F1_score = f1_score)
}

#Bootstrap confidence interval function
bootstrap_ci <- function(pred_prob, true_class, threshold, B = 1000) {
  n <- length(true_class)
  
  boot_metrics <- data.frame(
    Sensitivity = numeric(B),
    Specificity = numeric(B),
    PPV = numeric(B),
    NPV = numeric(B),
    CCR = numeric(B),
    F1_score = numeric(B),
    Brier_score = numeric(B)
  )
  
  for (b in 1:B) {
    idx <- sample(1:n, n, replace = TRUE)
    
    boot_prob <- pred_prob[idx]
    boot_true <- true_class[idx]
    
    metrics <- calculate_metrics(boot_prob, boot_true, threshold)
    
    boot_metrics[b, "Sensitivity"] <- metrics$Sensitivity
    boot_metrics[b, "Specificity"] <- metrics$Specificity
    boot_metrics[b, "PPV"] <- metrics$PPV
    boot_metrics[b, "NPV"] <- metrics$NPV
    boot_metrics[b, "CCR"] <- metrics$CCR
    boot_metrics[b, "F1_score"] <- metrics$F1_score
    boot_metrics[b, "Brier_score"] <- mean((boot_prob - boot_true)^2)
  }
  
  apply(boot_metrics, 2, quantile, probs = c(0.025, 0.975), na.rm = TRUE)
}

format_ci <- function(estimate, ci_vec) {
  sprintf("%.3f\n(%.3f-%.3f)", estimate, ci_vec[1], ci_vec[2])
}

#计算训练队列的性能指标
for (model_name in names(LH_train_preds)) {
  #获取训练队列模型的预测结果
  pred_train <- LH_train_preds[[model_name]]
  #获取训练队列模型的预测概率
  pred_prob_train <- pred_train$prob[, "Yes"]
  
  #存训练队列模型的预测概率到列表
  LH_train_prob_data[[model_name]] <- pred_prob_train
  
  #获取训练队列的实际分类
  true_class_train <- data_LH_train_std_num$CMDs
  true_class_train <- ifelse(true_class_train == "Yes", 1, 0)
  
  roc_train <- roc(true_class_train, pred_prob_train)
  auc_train <- roc_train$auc
  ci_train <- ci(roc_train)
  #合并AUC和95%CI，保留三位小数
  auc_with_ci_train <- sprintf("%.3f\n(%.3f-%.3f)", auc_train, ci_train[1], ci_train[3]) 
  
  #存储roc()结果到列表
  LH_train_roc_auc[[model_name]] <- roc_train
  
  #提取绘制ROC曲线的数据并添加到数据框中
  roc_data_train <- data.frame(
    FPR = 1-roc_train$specificities, #假阳性率FPR: 1-specificities
    TPR = roc_train$sensitivities, #真阳性率TPR: sensitivities
    model = model_name #模型名称
  )
  #模型名排序
  roc_data_train$model <- factor(roc_data_train$model, levels = c("LR", "RF", "XGBoost", "SVM", "KNN", "DT", "ANN"))
  #将测试队列各个模型的ROC数据合并
  LH_train_roc_data <- rbind(LH_train_roc_data, roc_data_train)
  
  ##计算训练队列的最佳阈值
  #定义阈值范围
  threshold <- seq(0, 1, by = 0.001)
  #对不同的阈值threshold计算性能指标
  metrics_list_train <- sapply(threshold, function(t) {
    calculate_metrics(pred_prob_train, true_class_train, t)
  }, simplify = F)
  
  distances <- sapply(metrics_list_train, function(metrics) {
    sqrt((1-metrics$Sensitivity)^2 + (1-metrics$Specificity)^2)
  })
  #找到训练队列的最佳阈值
  best_threshold_train <- threshold[which.min(distances)]
  
  #使用训练队列的最佳阈值计算训练队列的性能指标
  best_metrics_train <- calculate_metrics(pred_prob_train, true_class_train, best_threshold_train)
  
  #计算Brier score
  brier_score_train <- mean((pred_prob_train - true_class_train)^2)
  
  #Bootstrap CI
  set.seed(123)
  ci_train_metrics <- bootstrap_ci(pred_prob_train, true_class_train, best_threshold_train, B = 1000)
  
  Sensitivity_CI  <- format_ci(best_metrics_train$Sensitivity,  ci_train_metrics[, "Sensitivity"])
  Specificity_CI  <- format_ci(best_metrics_train$Specificity,  ci_train_metrics[, "Specificity"])
  PPV_CI          <- format_ci(best_metrics_train$PPV,          ci_train_metrics[, "PPV"])
  NPV_CI          <- format_ci(best_metrics_train$NPV,          ci_train_metrics[, "NPV"])
  CCR_CI          <- format_ci(best_metrics_train$CCR,          ci_train_metrics[, "CCR"])
  F1_score_CI     <- format_ci(best_metrics_train$F1_score,     ci_train_metrics[, "F1_score"])
  Brier_score_CI  <- format_ci(brier_score_train,               ci_train_metrics[, "Brier_score"])
  
  #汇总训练队列的模型结果
  train_metrics_result <- data.frame(
    Model = model_name,
    Dataset = "Training cohort",
    AUC_CI = auc_with_ci_train,
    Threshold = round(best_threshold_train, 3),
    Sensitivity_CI = Sensitivity_CI,
    Specificity_CI = Specificity_CI,
    PPV_CI = PPV_CI,
    NPV_CI = NPV_CI,
    CCR_CI = CCR_CI,
    F1_score_CI = F1_score_CI,
    Brier_score_CI = Brier_score_CI
  )
  #将训练队列各个模型的结果添加到数据框中
  LH_train_metrics <- rbind(LH_train_metrics, train_metrics_result)
  
  ##提取训练队列用于DCA数据
  #构造数据框：truth和预测概率
  train_truth_p <- data.frame(
    truth = true_class_train,  #将truth转换为0/1
    p = pred_prob_train  #提取预测概率
  )
  
  #计算决策曲线
  train_dca_result <- decision_curve(truth ~ p, data = train_truth_p, family = binomial, thresholds = seq(0, 1, by = 0.01))
  #提取数据并添加到数据框中
  train_thresholds <- train_dca_result$derived.data$thresholds
  train_net_benefit <- train_dca_result$derived.data$sNB
  train_model <- train_dca_result$derived.data$model
  train_model <- ifelse(train_model == "truth ~ p", model_name, train_model)
  #合并决策数据
  LH_train_dca_data <- rbind(LH_train_dca_data, data.frame(
    threshold = train_thresholds,
    net_benefit = train_net_benefit,
    model = train_model
  ))
  #模型名排序
  LH_train_dca_data$model <- factor(LH_train_dca_data$model, levels = c("LR", "RF", "XGBoost", "SVM", "KNN", "DT", "ANN", "All", "None"))
  
}

#计算测试队列的性能指标
for (model_name in names(LH_test_preds)) {
  #获取测试队列模型的预测结果
  pred_test <- LH_test_preds[[model_name]]
  #获取测试队列模型的预测概率
  pred_prob_test <- pred_test$prob[, "Yes"]
  
  #存测试队列模型的预测概率到列表
  LH_test_prob_data[[model_name]] <- pred_prob_test
  
  #获取测试队列的实际分类
  true_class_test <- data_LH_test_std_num$CMDs
  true_class_test <- ifelse(true_class_test == "Yes", 1, 0)
  
  #计算测试队列AUC及95%CI
  roc_test <- roc(true_class_test, pred_prob_test)
  auc_test <- roc_test$auc
  ci_test <- ci(roc_test)
  #合并AUC和95%CI，保留三位小数
  auc_with_ci_test <- sprintf("%.3f\n(%.3f-%.3f)", auc_test, ci_test[1], ci_test[3]) 
  
  #存储roc()结果到列表
  LH_test_roc_auc[[model_name]] <- roc_test
  
  #提取绘制ROC曲线的数据并添加到数据框中
  roc_data_test <- data.frame(
    FPR = 1-roc_test$specificities, #假阳性率FPR: 1-specificities
    TPR = roc_test$sensitivities, #真阳性率TPR: sensitivities
    model = model_name #模型名称
  )
  #模型名排序
  roc_data_test$model <- factor(roc_data_test$model, levels = c("LR", "RF", "XGBoost", "SVM", "KNN", "DT", "ANN"))
  #将测试队列各个模型的ROC数据合并
  LH_test_roc_data <- rbind(LH_test_roc_data, roc_data_test)
  
  #获取训练队列的最佳阈值
  best_threshold_train <- LH_train_metrics[LH_train_metrics$Model == model_name, "Threshold"]
  #使用训练队列的最佳阈值计算测试队列的性能
  best_metrics_test <- calculate_metrics(pred_prob_test, true_class_test, best_threshold_train)
  
  #计算测试队列的Brier score
  brier_score_test <- mean((pred_prob_test - true_class_test)^2)
  
  #Bootstrap CI
  set.seed(123)
  ci_test_metrics <- bootstrap_ci(pred_prob_test, true_class_test, best_threshold_train, B = 1000)
  
  Sensitivity_CI  <- format_ci(best_metrics_test$Sensitivity,  ci_test_metrics[, "Sensitivity"])
  Specificity_CI  <- format_ci(best_metrics_test$Specificity,  ci_test_metrics[, "Specificity"])
  PPV_CI          <- format_ci(best_metrics_test$PPV,          ci_test_metrics[, "PPV"])
  NPV_CI          <- format_ci(best_metrics_test$NPV,          ci_test_metrics[, "NPV"])
  CCR_CI          <- format_ci(best_metrics_test$CCR,          ci_test_metrics[, "CCR"])
  F1_score_CI     <- format_ci(best_metrics_test$F1_score,     ci_test_metrics[, "F1_score"])
  Brier_score_CI  <- format_ci(brier_score_test,               ci_test_metrics[, "Brier_score"])
  
  #汇总测试队列的模型结果到数据框中
  test_metrics_result <- data.frame(
    Model = model_name,
    Dataset = "Testing cohort",
    AUC_CI = auc_with_ci_test,
    Threshold = round(best_threshold_train, 3), #使用训练队列的最佳阈值
    Sensitivity_CI = Sensitivity_CI,
    Specificity_CI = Specificity_CI,
    PPV_CI = PPV_CI,
    NPV_CI = NPV_CI,
    CCR_CI = CCR_CI,
    F1_score_CI = F1_score_CI,
    Brier_score_CI = Brier_score_CI
  )
  #将测试队列各个模型的结果合并
  LH_test_metrics <- rbind(LH_test_metrics, test_metrics_result)
  
  ##提取测试队列用于DCA数据
  #构造数据框：truth和预测概率
  test_truth_p <- data.frame(
    truth = true_class_test,  #将truth转换为0/1
    p = pred_prob_test  #提取预测概率
  )
  
  #计算决策曲线
  test_dca_result <- decision_curve(truth ~ p, data = test_truth_p, family = binomial, thresholds = seq(0, 1, by = 0.01))
  #提取数据并添加到数据框中
  test_thresholds <- test_dca_result$derived.data$thresholds
  test_net_benefit <- test_dca_result$derived.data$sNB
  test_model <- test_dca_result$derived.data$model
  test_model <- ifelse(test_model == "truth ~ p", model_name, test_model)
  #合并决策数据
  LH_test_dca_data <- rbind(LH_test_dca_data, data.frame(
    threshold = test_thresholds,
    net_benefit = test_net_benefit,
    model = test_model
  ))
  #模型名排序
  LH_test_dca_data$model <- factor(LH_test_dca_data$model, levels = c("LR", "RF", "XGBoost", "SVM", "KNN", "DT", "ANN", "All", "None"))
  
}


##-----delong test-----
#初始化一个空的数据框来存储测试队列的delong test结果
LH_test_auc_comparisons <- data.frame()
#获取训练队列模型名称的组合
model_names <- names(LH_test_roc_auc)

#P value format
pvalue_format <- Vectorize(function(p) {
  if (is.na(p)) return("")
  
  if (p < 0.001) {
    return("<0.001") 
  } else {
    return(sprintf("%.3f", p))
  }
})

#两两比较ROC曲线，并提取测试队列的Z值和p-value
for (i in 1:(length(model_names) - 1)) {
  for (j in (i + 1):length(model_names)) {
    #获取当前两个模型的ROC曲线数据
    test_roc1 <- LH_test_roc_auc[[model_names[i]]]
    test_roc2 <- LH_test_roc_auc[[model_names[j]]]
    #DeLong检验
    test_delong_test <- roc.test(test_roc1, test_roc2, method = "delong")
    #提取Z值和p-value，分别保留三位小数和自定义函数
    test_Z_score <- round(test_delong_test$statistic, 3)
    test_P_value <- pvalue_format(test_delong_test$p.value)
    
    #将结果添加到数据框并合并
    LH_test_auc_comparisons <- rbind(LH_test_auc_comparisons, data.frame(
      Model_comparison = paste(model_names[i], model_names[j], sep = " vs. "),
      LH_test_Z_score = test_Z_score,
      LH_test_P_value = test_P_value
    ))
  }
}


#-----绘制测试集所有模型的ROC曲线-----
LH_test_roc <- ggplot(LH_test_roc_data, aes(x = FPR, y = TPR, color = model)) +
  geom_line(size = 0.1) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.2)) + # 设置x轴刻度
  scale_y_continuous(breaks = seq(0, 1, by = 0.2)) + # 设置y轴刻度
  #自定义颜色和图例标签
  scale_color_manual(
    values = c("#4DAF9C", "#807DBA", "#E34A33", "#3182BD", "#A63603", "#31A354", "#8C6D31"),
    labels = c(
      expression("LR (AUC=0.791)"),
      expression("RF (AUC=0.801, " * italic(P) * "=0.380)"),
      expression("XGBoost (AUC=0.806, " * italic(P) * "=0.133)"),
      expression("SVM (AUC=0.778, " * italic(P) * "=0.291)"),
      expression("KNN (AUC=0.763, " * italic(P) * "=0.026)"),
      expression("DT (AUC=0.665, " * italic(P) * "<0.001)"),
      expression("ANN (AUC=0.776, " * italic(P) * "=0.114)"))
  ) + 
  labs(x = "1-Specificity",
       y = "Sensitivity",
       title = "ROC Curve", #图标题
       color = "") + #图例标题为空
  theme_minimal() + #设置图表的主题
  theme(
    plot.background = element_rect(fill = "white", color = NA), #整体图像背景，边框无颜色
    panel.grid = element_blank(), #去掉网格线
    panel.border = element_rect(color = "black", fill = NA), #添加边框
    axis.ticks.x = element_line(color = "black"), #x轴刻度线
    axis.ticks.y = element_line(color = "black"), #y轴刻度线
    axis.ticks.length = unit(0.1, "cm"), #刻度线在外侧及长度
    legend.position = c(0.75, 0.22), #图例放置右下角
    legend.key.height = unit(0.2, "cm"), #控制每个图例项的高度
    legend.key.width = unit(0.4, "cm"), #控制每个图例项的宽度
    axis.title = element_text(size = 60), #坐标轴标题字体大小
    axis.text = element_text(size = 50), #坐标轴刻度字体大小
    legend.text = element_text(size = 25), #图例字体大小
    plot.title = element_text(size = 60, hjust = 0.5), #图标题字体、居中
    plot.tag = element_text(size = 90, face = "bold") #设置标签文字大小
  ) +
  coord_fixed(ratio = 5/7) + #设置图形为长方形
  annotate(geom = "segment", x = 0, y = 0, xend = 1, yend = 1, color = "black", size = 0.2, linetype = "dotted") #添加对角线


##-----绘制测试队列校准曲线图-----
#提取测试队列任务数据
LH_test_task_data <- LH_test_task$data()
LH_test_task_data$CMDs <- factor(ifelse(LH_test_task_data$CMDs == "Yes", 1, 0))

#对模型进行评分
LH_test_score <- Score(list(LR = LH_test_prob_data[["LR"]],
                            RF = LH_test_prob_data[["RF"]],
                            XGBoost = LH_test_prob_data[["XGBoost"]],
                            SVM = LH_test_prob_data[["SVM"]],
                            KNN = LH_test_prob_data[["KNN"]],
                            DT = LH_test_prob_data[["DT"]],
                            ANN = LH_test_prob_data[["ANN"]]),
                       formula = CMDs ~ 1, #模型评估公式
                       null.model = F, #表示不使用空模型进行比较
                       plots = "calibration", #绘制校准曲线
                       data = LH_test_task_data)

#提取校准曲线数据
LH_test_calibration_plot <- plotCalibration(LH_test_score, method = "nne", bandwidth = 0.05, plot = FALSE)
LH_test_calibration_data <- imap_dfr(LH_test_calibration_plot$plotFrames, ~ {
  .x %>% 
    as_tibble() %>% 
    mutate(model = .y)  #.y是列表元素名（lr/rf/svm等）
})
LH_test_calibration_data$model <- factor(LH_test_calibration_data$model, levels = c("LR", "RF", "XGBoost", "SVM", "KNN", "DT", "ANN"))

#创建一条自定义的线段数据，用于绘制完美校准线
segment_data <- data.frame(
  x = 0, y = 0, xend = 1,  yend = 1, 
  segment_type = "Ideal"  #用于图例标记线条
)

#绘制校准曲线图
LH_test_calibration <- ggplot(LH_test_calibration_data, aes(x = Pred, y = Obs, color = model)) +
  geom_line(linewidth = 0.1) + #设置线的粗细
  #使用 geom_segment() 绘制完美校准线
  geom_segment(data = segment_data, aes(x = x, y = y, xend = xend, yend = yend, color = segment_type), linewidth = 0.2, linetype = "dotted") +
  scale_x_continuous(limits = c(0, 1),  #限制x轴刻度
                     breaks = seq(0, 1, by = 0.2), name = "Predicted Probability") + #设置x轴刻度和标签
  scale_y_continuous(limits = c(0, 1),  #限制y轴刻度
                     breaks = seq(0, 1, by = 0.2), name = "Actual Probability") + #设置y轴刻度和标签
  #自定义颜色和图例标签
  scale_color_manual(
    values = c("#4DAF9C", "#807DBA", "#E34A33", "#3182BD", "#A63603", "#31A354", "#8C6D31", "black"),
    labels = c("LR", "RF", "XGBoost", "SVM", "KNN", "DT", "ANN", "Ideal")
  ) +
  labs(title = "Calibration Curve", #图标题
       color = "") + #图例标题为空
  theme_minimal() + #设置图表的主题
  theme(
    plot.background = element_rect(fill = "white", color = NA), #整体图像背景，边框无颜色
    panel.grid = element_blank(), #去掉网格线
    panel.border = element_rect(color = "black", fill = NA), #添加边框
    axis.ticks.x = element_line(color = "black"), #x轴刻度线
    axis.ticks.y = element_line(color = "black"), #y轴刻度线
    axis.ticks.length = unit(0.1, "cm"), #刻度线在外侧及长度
    legend.position = c(0.15, 0.82), #图例放置左上角
    legend.key.height = unit(0.2, "cm"), #控制每个图例项的高度
    legend.key.width = unit(0.4, "cm"), #控制每个图例项的宽度
    axis.title = element_text(size = 60), #坐标轴标题字体大小
    axis.text = element_text(size = 50), #坐标轴刻度字体大小
    legend.text = element_text(size = 30), #图例字体大小
    plot.title = element_text(size = 60, hjust = 0.5), #图标题字体、居中
    plot.tag = element_text(size = 90, face = "bold") #设置标签文字大小
  ) +
  coord_fixed(ratio = 5/7) #设置图形为长方形


#-----绘制测试队列DCA图-----
LH_test_dca <- ggplot(LH_test_dca_data, aes(x = threshold, y = net_benefit, color = model)) +
  geom_line(linewidth = 0.1) + #设置线的粗细
  scale_x_continuous(limits = c(0, 0.8), #限制x轴刻度
                     breaks = seq(0, 0.8, by = 0.2), #设置x轴刻度
                     name = "High Risk Threshold") + #设置x轴标签
  scale_y_continuous(limits = c(-0.2, 0.8), #限制y轴刻度
                     breaks = seq(-0.2, 0.8, by = 0.2), #设置y轴刻度
                     name = "Standardized Net Benefit") + #设置y轴标签
  scale_color_manual(values = c("#4DAF9C", "#807DBA", "#E34A33", "#3182BD", 
                                "#A63603", "#31A354", "#8C6D31", "gray", "black")) + #自定义颜色
  labs(title = "DCA", #图标题
       color = "") + #图例标题为空
  theme_minimal() + #设置图表的主题
  theme(
    plot.background = element_rect(fill = "white", color = NA), #整体图像背景，边框无颜色
    panel.grid = element_blank(), #去掉网格线
    panel.border = element_rect(color = "black", fill = NA), #添加边框
    axis.ticks.x = element_line(color = "black"), #x轴刻度线
    axis.ticks.y = element_line(color = "black"), #y轴刻度线
    axis.ticks.length = unit(0.1, "cm"), #刻度线在外侧及长度
    legend.position = c(0.86, 0.79), #图例放置右上角
    legend.key.height = unit(0.2, "cm"), #控制每个图例项的高度
    legend.key.width = unit(0.4, "cm"), #控制每个图例项的宽度
    axis.title = element_text(size = 60), #坐标轴标题字体大小
    axis.text = element_text(size = 50), #坐标轴刻度字体大小
    legend.text = element_text(size = 30), #图例字体大小
    plot.title = element_text(size = 60, hjust = 0.5), #图标题字体、居中
    plot.tag = element_text(size = 90, face = "bold") #设置标签文字大小
  ) +
  coord_fixed(ratio = 4/7) #设置图形为长方形


##-----绘制测试队列雷达图-----
#将数据转换为长格式，适合绘制雷达图
LH_test_radar_data <- LH_test_metrics %>%
  select(3, 5:11) %>%
  mutate(across(ends_with("_CI"),
                ~ as.numeric(sub("\\s*\\(.*$", "", .x))))
LH_test_radar_data <- t(LH_test_radar_data)  #转置数据框
LH_test_radar_data <- as.data.frame(LH_test_radar_data)  #转换回数据框
colnames(LH_test_radar_data) <- c("LR", "RF", "XGBoost", "SVM", "KNN", "DT", "ANN") #添加列名
LH_test_radar_data$metrics <- c("AUC", "Sensitivity", "Specificity", "PPV", "NPV", "CCR", "F1 score", "Brier score") #添加性能列
LH_test_radar_data <- LH_test_radar_data[, c("metrics", "LR", "RF", "XGBoost", "SVM", "KNN", "DT", "ANN")] #排序
LH_test_radar_data$metrics <- factor(LH_test_radar_data$metrics, levels=c("AUC", "Sensitivity", "Specificity", "PPV", "NPV", "CCR", "F1 score", "Brier score")) #转换为因子
#绘制雷达图-AUC
LH_test_AUC <- ggradar(LH_test_radar_data[1,], 
                       grid.line.width = 0.4, #网格线宽度
                       grid.label.size = 11, #网格线标签字体大小
                       axis.label.size = 9, #轴标签字体大小
                       grid.min = 0, #网格线最小值
                       grid.mid = 0.45, #网格线均值
                       grid.max = 0.9, #网格线最大值
                       values.radar = c(0, 0.45, 0.9), #轴标签显示
                       background.circle.colour = "white", #背景颜色
                       group.line.width = 0.5, #线宽
                       group.point.size = 1, #数据点大小
                       fill = T, #填充色
                       fill.alpha = 0.3, #填充色不透明度
                       group.colours = "#95B0E0") +
  ggtitle("AUC") + #添加标题
  theme(plot.title = element_text(hjust = 0.5, size = 40), #hjust=0.5标题居中，设置标签文字大小
        plot.margin = margin(0, 0, 0, 0, "cm"), #调整边距
        plot.tag = element_text(size = 50, face = "bold") #右上角标签字体大小 
  ) +
  coord_fixed(ratio = 1) #设置图形为正方形

#绘制雷达图-Sensitivity
LH_test_Sensitivity <- ggradar(LH_test_radar_data[2,], 
                               grid.line.width = 0.4, #网格线宽度
                               grid.label.size = 11, #网格线标签字体大小
                               axis.label.size = 9, #轴标签字体大小
                               grid.min = 0, #网格线最小值
                               grid.mid = 0.4, #网格线均值
                               grid.max = 0.8, #网格线最大值
                               values.radar = c(0, 0.4, 0.8), #轴标签显示
                               background.circle.colour = "white", #背景颜色
                               group.line.width = 0.5, #线宽
                               group.point.size = 1, #数据点大小
                               fill = T, #填充色
                               fill.alpha = 0.3, #填充色不透明度
                               group.colours = "#56AEDE") +
  ggtitle("Sensitivity") +#添加标题
  theme(plot.title = element_text(hjust = 0.5, size = 40), #hjust=0.5标题居中，设置标签文字大小
        plot.margin = margin(0, 0, 0, 0, "cm"), #调整边距
        plot.tag = element_text(size = 50, face = "bold") #右上角标签字体大小 
  ) +
  coord_fixed(ratio = 1) #设置图形为正方形

#绘制雷达图-Specificity
LH_test_Specificity <- ggradar(LH_test_radar_data[3,], 
                               grid.line.width = 0.4, #网格线宽度
                               grid.label.size = 11, #网格线标签字体大小
                               axis.label.size = 9, #轴标签字体大小
                               grid.min = 0, #网格线最小值
                               grid.mid = 0.5, #网格线均值
                               grid.max = 1, #网格线最大值
                               values.radar = c(0, 0.5, 1), #轴标签显示
                               background.circle.colour = "white", #背景颜色
                               group.line.width = 0.5, #线宽
                               group.point.size = 1, #数据点大小
                               fill = T, #填充色
                               fill.alpha = 0.2, #填充色不透明度
                               group.colours = "#EE7A5F") +
  ggtitle("Specificity") + #添加标题
  theme(plot.title = element_text(hjust = 0.5, size = 40), #hjust=0.5标题居中，设置标签文字大小
        plot.margin = margin(0, 0, 0, 0, "cm"), #调整边距
        plot.tag = element_text(size = 50, face = "bold") #右上角标签字体大小 
  ) +
  coord_fixed(ratio = 1) #设置图形为正方形

#绘制雷达图-PPV
LH_test_PPV <- ggradar(LH_test_radar_data[4,], 
                       grid.line.width = 0.4, #网格线宽度
                       grid.label.size = 11, #网格线标签字体大小
                       axis.label.size = 9, #轴标签字体大小
                       grid.min = 0, #网格线最小值
                       grid.mid = 0.3, #网格线均值
                       grid.max = 0.6, #网格线最大值
                       values.radar = c(0, 0.3, 0.6), #轴标签显示
                       background.circle.colour = "white", #背景颜色
                       group.line.width = 0.5, #线宽
                       group.point.size = 1, #数据点大小
                       fill = T, #填充色
                       fill.alpha = 0.2, #填充色不透明度
                       group.colours = "#FEAF8A") +
  ggtitle("PPV") + #添加标题
  theme(plot.title = element_text(hjust = 0.5, size = 40), #hjust=0.5标题居中，设置标签文字大小
        plot.margin = margin(0, 0, 0, 0, "cm"), #调整边距
        plot.tag = element_text(size = 50, face = "bold") #右上角标签字体大小 
  ) +
  coord_fixed(ratio = 1) #设置图形为正方形

#绘制雷达图-NPV
LH_test_NPV <- ggradar(LH_test_radar_data[5,], 
                       grid.line.width = 0.4, #网格线宽度
                       grid.label.size = 11, #网格线标签字体大小
                       axis.label.size = 9, #轴标签字体大小
                       grid.min = 0, #网格线最小值
                       grid.mid = 0.5, #网格线均值
                       grid.max = 1, #网格线最大值
                       values.radar = c(0, 0.5, 1), #轴标签显示
                       background.circle.colour = "white", #背景颜色
                       group.line.width = 0.5, #线宽
                       group.point.size = 1, #数据点大小
                       fill = T, #填充色
                       fill.alpha = 0.3, #填充色不透明度
                       group.colours = "#F6B7C6") +
  ggtitle("NPV") + #添加标题
  theme(plot.title = element_text(hjust = 0.5, size = 40), #hjust=0.5标题居中，设置标签文字大小
        plot.margin = margin(0, 0, 0, 0, "cm"), #调整边距
        plot.tag = element_text(size = 50, face = "bold") #右上角标签字体大小 
  ) +
  coord_fixed(ratio = 1) #设置图形为正方形

#绘制雷达图-CCR
LH_test_CCR <- ggradar(LH_test_radar_data[6,], 
                       grid.line.width = 0.4, #网格线宽度
                       grid.label.size = 11, #网格线标签字体大小
                       axis.label.size = 9, #轴标签字体大小
                       grid.min = 0, #网格线最小值
                       grid.mid = 0.5, #网格线均值
                       grid.max = 1, #网格线最大值
                       values.radar = c(0, 0.5, 1), #轴标签显示
                       background.circle.colour = "white", #背景颜色
                       group.line.width = 0.5, #线宽
                       group.point.size = 1, #数据点大小
                       fill = T, #填充色
                       fill.alpha = 0.3, #填充色不透明度
                       group.colours = "#D8CBF0") +
  ggtitle("CCR") + #添加标题
  theme(plot.title = element_text(hjust = 0.5, size = 40), #hjust=0.5标题居中，设置标签文字大小
        plot.margin = margin(0, 0, 0, 0, "cm"), #调整边距
        plot.tag = element_text(size = 50, face = "bold") #右上角标签字体大小 
  ) +
  coord_fixed(ratio = 1) #设置图形为正方形

#绘制雷达图-F1 score
LH_test_F1_score <- ggradar(LH_test_radar_data[7,], 
                            grid.line.width = 0.4, #网格线宽度
                            grid.label.size = 11, #网格线标签字体大小
                            axis.label.size = 9, #轴标签字体大小
                            grid.min = 0, #网格线最小值
                            grid.mid = 0.25, #网格线均值
                            grid.max = 0.5, #网格线最大值
                            values.radar = c(0, 0.25, 0.5), #轴标签显示
                            background.circle.colour = "white", #背景颜色
                            group.line.width = 0.5, #线宽
                            group.point.size = 1, #数据点大小
                            fill = T, #填充色
                            fill.alpha = 0.3, #填充色不透明度
                            group.colours = "#9FCDC9") +
  ggtitle("F1 score") + #添加标题
  theme(plot.title = element_text(hjust = 0.5, size = 40), #hjust=0.5标题居中，设置标签文字大小
        plot.margin = margin(0, 0, 0, 0, "cm"), #调整边距
        plot.tag = element_text(size = 50, face = "bold") #右上角标签字体大小 
  ) +
  coord_fixed(ratio = 1) #设置图形为正方形

#绘制雷达图-Brier score
LH_test_Brier_score <- ggradar(LH_test_radar_data[8,], 
                               grid.line.width = 0.4, #网格线宽度
                               grid.label.size = 11, #网格线标签字体大小
                               axis.label.size = 9, #轴标签字体大小
                               grid.min = 0, #网格线最小值
                               grid.mid = 0.05, #网格线均值
                               grid.max = 0.1, #网格线最大值
                               values.radar = c(0, 0.05, 0.1), #轴标签显示
                               background.circle.colour = "white", #背景颜色
                               group.line.width = 0.5, #线宽
                               group.point.size = 1, #数据点大小
                               fill = T, #填充色
                               fill.alpha = 0.2, #填充色不透明度
                               group.colours = "#7EB87B") +
  ggtitle("Brier score") + #添加标题
  theme(plot.title = element_text(hjust = 0.5, size = 40), #hjust=0.5标题居中，设置标签文字大小
        plot.margin = margin(0, 0, 0, 0, "cm"), #调整边距
        plot.tag = element_text(size = 50, face = "bold") #右上角标签字体大小 
  ) +
  coord_fixed(ratio = 1) #设置图形为正方形

#合并测试队列各性能雷达图
LH_test_radar <- (LH_test_AUC + LH_test_Sensitivity + LH_test_Specificity + LH_test_PPV + 
                    LH_test_NPV + LH_test_CCR + LH_test_F1_score + LH_test_Brier_score) + 
  plot_annotation(tag_levels = "A") + #自动添加A和B标签
  plot_layout(ncol = 4, nrow = 2) #图片2*4

#导出测试队列各性能雷达图
ggsave(plot = LH_test_radar, 
       filename = "LH_test_radar.png", 
       width = 17, 
       height = 8.5,
       units = "cm", 
       dpi = 600)


##-----模型性能评估：Unstable和High-stable组-----
#创建训练队列和测试队列的模型预测结果列表
UH_train_preds <- list(
  LR = UH_train_lr_pred, 
  RF = UH_train_rf_pred, 
  XGBoost = UH_train_xgb_pred, 
  SVM = UH_train_svm_pred, 
  KNN = UH_train_knn_pred, 
  DT = UH_train_dt_pred, 
  ANN = UH_train_ann_pred
)
UH_test_preds <- list(
  LR = UH_test_lr_pred, 
  RF = UH_test_rf_pred, 
  XGBoost = UH_test_xgb_pred, 
  SVM = UH_test_svm_pred, 
  KNN = UH_test_knn_pred, 
  DT = UH_test_dt_pred, 
  ANN = UH_test_ann_pred
)

#创建空的数据框，分别用于存储训练队列和测试队列的性能评估结果
UH_train_metrics <- data.frame()
UH_test_metrics <- data.frame()

#创建空的数据框，分别用于存储绘制训练队列和测试队列的ROC数据
UH_train_roc_data <- data.frame()
UH_test_roc_data <- data.frame()

#创建列表来存储训练队列和测试队列的ROC()结果，后面进行AUC的DeLong检验
UH_train_roc_auc <- list()
UH_test_roc_auc <- list()

#创建列表来存储训练队列和测试队列模型的预测概率数据（校准曲线）
UH_train_prob_data <- list()
UH_test_prob_data <- list()

#创建空的数据框，分别用于存储训练队列和测试队列的dca数据
UH_train_dca_data <- data.frame()
UH_test_dca_data <- data.frame()

#计算训练队列的性能指标
for (model_name in names(UH_train_preds)) {
  #获取训练队列模型的预测结果
  pred_train <- UH_train_preds[[model_name]]
  #获取训练队列模型的预测概率
  pred_prob_train <- pred_train$prob[, "Yes"]
  
  #存训练队列模型的预测概率到列表
  UH_train_prob_data[[model_name]] <- pred_prob_train
  
  #获取训练队列的实际分类
  true_class_train <- pred_train$truth
  true_class_train <- ifelse(true_class_train == "Yes", 1, 0)
  
  roc_train <- roc(true_class_train, pred_prob_train)
  auc_train <- roc_train$auc
  ci_train <- ci(roc_train)
  #合并AUC和95%CI，保留三位小数
  auc_with_ci_train <- sprintf("%.3f\n(%.3f-%.3f)", auc_train, ci_train[1], ci_train[3]) 
  
  #存储roc()结果到列表
  UH_train_roc_auc[[model_name]] <- roc_train
  
  #提取绘制ROC曲线的数据并添加到数据框中
  roc_data_train <- data.frame(
    FPR = 1-roc_train$specificities, #假阳性率FPR: 1-specificities
    TPR = roc_train$sensitivities, #真阳性率TPR: sensitivities
    model = model_name #模型名称
  )
  #模型名排序
  roc_data_train$model <- factor(roc_data_train$model, levels = c("LR", "RF", "XGBoost", "SVM", "KNN", "DT", "ANN"))
  #将测试队列各个模型的ROC数据合并
  UH_train_roc_data <- rbind(UH_train_roc_data, roc_data_train)
  
  ##计算训练队列的最佳阈值
  #定义阈值范围
  threshold <- seq(0, 1, by = 0.001)
  #对不同的阈值threshold计算性能指标
  metrics_list_train <- sapply(threshold, function(t) {
    calculate_metrics(pred_prob_train, true_class_train, t)
  }, simplify = F)
  
  distances <- sapply(metrics_list_train, function(metrics) {
    sqrt((1-metrics$Sensitivity)^2 + (1-metrics$Specificity)^2)
  })
  #找到训练队列的最佳阈值
  best_threshold_train <- threshold[which.min(distances)]
  
  #使用训练队列的最佳阈值计算训练队列的性能指标
  best_metrics_train <- calculate_metrics(pred_prob_train, true_class_train, best_threshold_train)
  
  #计算Brier score
  brier_score_train <- mean((pred_prob_train - true_class_train)^2)
  
  #Bootstrap CI
  set.seed(123)
  ci_train_metrics <- bootstrap_ci(pred_prob_train, true_class_train, best_threshold_train, B = 1000)
  
  Sensitivity_CI  <- format_ci(best_metrics_train$Sensitivity,  ci_train_metrics[, "Sensitivity"])
  Specificity_CI  <- format_ci(best_metrics_train$Specificity,  ci_train_metrics[, "Specificity"])
  PPV_CI          <- format_ci(best_metrics_train$PPV,          ci_train_metrics[, "PPV"])
  NPV_CI          <- format_ci(best_metrics_train$NPV,          ci_train_metrics[, "NPV"])
  CCR_CI          <- format_ci(best_metrics_train$CCR,          ci_train_metrics[, "CCR"])
  F1_score_CI     <- format_ci(best_metrics_train$F1_score,     ci_train_metrics[, "F1_score"])
  Brier_score_CI  <- format_ci(brier_score_train,               ci_train_metrics[, "Brier_score"])
  
  #汇总训练队列的模型结果
  train_metrics_result <- data.frame(
    Model = model_name,
    Dataset = "Training cohort",
    AUC_CI = auc_with_ci_train,
    Threshold = round(best_threshold_train, 3),
    Sensitivity_CI = Sensitivity_CI,
    Specificity_CI = Specificity_CI,
    PPV_CI = PPV_CI,
    NPV_CI = NPV_CI,
    CCR_CI = CCR_CI,
    F1_score_CI = F1_score_CI,
    Brier_score_CI = Brier_score_CI
  )
  #将训练队列各个模型的结果添加到数据框中
  UH_train_metrics <- rbind(UH_train_metrics, train_metrics_result)
  
  ##提取训练队列用于DCA数据
  #构造数据框：truth和预测概率
  train_truth_p <- data.frame(
    truth = true_class_train,  #将truth转换为0/1
    p = pred_prob_train  #提取预测概率
  )
  
  #计算决策曲线
  train_dca_result <- decision_curve(truth ~ p, data = train_truth_p, family = binomial, thresholds = seq(0, 1, by = 0.01))
  #提取数据并添加到数据框中
  train_thresholds <- train_dca_result$derived.data$thresholds
  train_net_benefit <- train_dca_result$derived.data$sNB
  train_model <- train_dca_result$derived.data$model
  train_model <- ifelse(train_model == "truth ~ p", model_name, train_model)
  #合并决策数据
  UH_train_dca_data <- rbind(UH_train_dca_data, data.frame(
    threshold = train_thresholds,
    net_benefit = train_net_benefit,
    model = train_model
  ))
  #模型名排序
  UH_train_dca_data$model <- factor(UH_train_dca_data$model, levels = c("LR", "RF", "XGBoost", "SVM", "KNN", "DT", "ANN", "All", "None"))
  
}

#计算测试队列的性能指标
for (model_name in names(UH_test_preds)) {
  #获取测试队列模型的预测结果
  pred_test <- UH_test_preds[[model_name]]
  #获取测试队列模型的预测概率
  pred_prob_test <- pred_test$prob[, "Yes"]
  
  #存测试队列模型的预测概率到列表
  UH_test_prob_data[[model_name]] <- pred_prob_test
  
  #获取测试队列的实际分类
  true_class_test <- pred_test$truth
  true_class_test <- ifelse(true_class_test == "Yes", 1, 0)
  
  #计算测试队列AUC及95%CI
  roc_test <- roc(true_class_test, pred_prob_test)
  auc_test <- roc_test$auc
  ci_test <- ci(roc_test)
  #合并AUC和95%CI，保留三位小数
  auc_with_ci_test <- sprintf("%.3f\n(%.3f-%.3f)", auc_test, ci_test[1], ci_test[3]) 
  
  #存储roc()结果到列表
  UH_test_roc_auc[[model_name]] <- roc_test
  
  #提取绘制ROC曲线的数据并添加到数据框中
  roc_data_test <- data.frame(
    FPR = 1-roc_test$specificities, #假阳性率FPR: 1-specificities
    TPR = roc_test$sensitivities, #真阳性率TPR: sensitivities
    model = model_name #模型名称
  )
  #模型名排序
  roc_data_test$model <- factor(roc_data_test$model, levels = c("LR", "RF", "XGBoost", "SVM", "KNN", "DT", "ANN"))
  #将测试队列各个模型的ROC数据合并
  UH_test_roc_data <- rbind(UH_test_roc_data, roc_data_test)
  
  #获取训练队列的最佳阈值
  best_threshold_train <- UH_train_metrics[UH_train_metrics$Model == model_name, "Threshold"]
  #使用训练队列的最佳阈值计算测试队列的性能
  best_metrics_test <- calculate_metrics(pred_prob_test, true_class_test, best_threshold_train)
  
  #计算测试队列的Brier score
  brier_score_test <- mean((pred_prob_test - true_class_test)^2)
  
  #Bootstrap CI
  set.seed(123)
  ci_test_metrics <- bootstrap_ci(pred_prob_test, true_class_test, best_threshold_train, B = 1000)
  
  Sensitivity_CI  <- format_ci(best_metrics_test$Sensitivity,  ci_test_metrics[, "Sensitivity"])
  Specificity_CI  <- format_ci(best_metrics_test$Specificity,  ci_test_metrics[, "Specificity"])
  PPV_CI          <- format_ci(best_metrics_test$PPV,          ci_test_metrics[, "PPV"])
  NPV_CI          <- format_ci(best_metrics_test$NPV,          ci_test_metrics[, "NPV"])
  CCR_CI          <- format_ci(best_metrics_test$CCR,          ci_test_metrics[, "CCR"])
  F1_score_CI     <- format_ci(best_metrics_test$F1_score,     ci_test_metrics[, "F1_score"])
  Brier_score_CI  <- format_ci(brier_score_test,               ci_test_metrics[, "Brier_score"])
  
  #汇总测试队列的模型结果到数据框中
  test_metrics_result <- data.frame(
    Model = model_name,
    Dataset = "Testing cohort",
    AUC_CI = auc_with_ci_test,
    Threshold = round(best_threshold_train, 3), #使用训练队列的最佳阈值
    Sensitivity_CI = Sensitivity_CI,
    Specificity_CI = Specificity_CI,
    PPV_CI = PPV_CI,
    NPV_CI = NPV_CI,
    CCR_CI = CCR_CI,
    F1_score_CI = F1_score_CI,
    Brier_score_CI = Brier_score_CI
  )
  #将测试队列各个模型的结果合并
  UH_test_metrics <- rbind(UH_test_metrics, test_metrics_result)
  
  ##提取测试队列用于DCA数据
  #构造数据框：truth和预测概率
  test_truth_p <- data.frame(
    truth = true_class_test,  #将truth转换为0/1
    p = pred_prob_test  #提取预测概率
  )
  
  #计算决策曲线
  test_dca_result <- decision_curve(truth ~ p, data = test_truth_p, family = binomial, thresholds = seq(0, 1, by = 0.01))
  #提取数据并添加到数据框中
  test_thresholds <- test_dca_result$derived.data$thresholds
  test_net_benefit <- test_dca_result$derived.data$sNB
  test_model <- test_dca_result$derived.data$model
  test_model <- ifelse(test_model == "truth ~ p", model_name, test_model)
  #合并决策数据
  UH_test_dca_data <- rbind(UH_test_dca_data, data.frame(
    threshold = test_thresholds,
    net_benefit = test_net_benefit,
    model = test_model
  ))
  #模型名排序
  UH_test_dca_data$model <- factor(UH_test_dca_data$model, levels = c("LR", "RF", "XGBoost", "SVM", "KNN", "DT", "ANN", "All", "None"))
  
}


##-----delong test-----
#初始化一个空的数据框来存储测试队列的delong test结果
UH_test_auc_comparisons <- data.frame()
#获取训练队列模型名称的组合
model_names <- names(UH_test_roc_auc)

#两两比较ROC曲线，并提取测试队列的Z值和p-value
for (i in 1:(length(model_names) - 1)) {
  for (j in (i + 1):length(model_names)) {
    #获取当前两个模型的ROC曲线数据
    test_roc1 <- UH_test_roc_auc[[model_names[i]]]
    test_roc2 <- UH_test_roc_auc[[model_names[j]]]
    #DeLong检验
    test_delong_test <- roc.test(test_roc1, test_roc2, method = "delong")
    #提取Z值和p-value，分别保留三位小数和自定义函数
    test_Z_score <- round(test_delong_test$statistic, 3)
    test_P_value <- pvalue_format(test_delong_test$p.value)
    
    #将结果添加到数据框并合并
    UH_test_auc_comparisons <- rbind(UH_test_auc_comparisons, data.frame(
      Model_comparison = paste(model_names[i], model_names[j], sep = " vs. "),
      UH_test_Z_score = test_Z_score,
      UH_test_P_value = test_P_value
    ))
  }
}


#-----绘制测试集所有模型的ROC曲线-----
UH_test_roc <- ggplot(UH_test_roc_data, aes(x = FPR, y = TPR, color = model)) +
  geom_line(size = 0.1) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.2)) + # 设置x轴刻度
  scale_y_continuous(breaks = seq(0, 1, by = 0.2)) + # 设置y轴刻度
  #自定义颜色和图例标签
  scale_color_manual(
    values = c("#4DAF9C", "#807DBA", "#E34A33", "#3182BD", "#A63603", "#31A354", "#8C6D31"),
    labels = c(
      expression("LR (AUC=0.797)"),
      expression("RF (AUC=0.804, " * italic(P) * "=0.422)"),
      expression("XGBoost (AUC=0.805, " * italic(P) * "=0.199)"),
      expression("SVM (AUC=0.732, " * italic(P) * "<0.001)"),
      expression("KNN (AUC=0.781, " * italic(P) * "=0.105)"),
      expression("DT (AUC=0.652, " * italic(P) * "<0.001)"),
      expression("ANN (AUC=0.794, " * italic(P) * "=0.091)"))
  ) + 
  labs(x = "1-Specificity",
       y = "Sensitivity",
       title = "ROC Curve", #图标题
       color = "") + #图例标题为空
  theme_minimal() + #设置图表的主题
  theme(
    plot.background = element_rect(fill = "white", color = NA), #整体图像背景，边框无颜色
    panel.grid = element_blank(), #去掉网格线
    panel.border = element_rect(color = "black", fill = NA), #添加边框
    axis.ticks.x = element_line(color = "black"), #x轴刻度线
    axis.ticks.y = element_line(color = "black"), #y轴刻度线
    axis.ticks.length = unit(0.1, "cm"), #刻度线在外侧及长度
    legend.position = c(0.75, 0.22), #图例放置右下角
    legend.key.height = unit(0.2, "cm"), #控制每个图例项的高度
    legend.key.width = unit(0.4, "cm"), #控制每个图例项的宽度
    axis.title = element_text(size = 60), #坐标轴标题字体大小
    axis.text = element_text(size = 50), #坐标轴刻度字体大小
    legend.text = element_text(size = 25), #图例字体大小
    plot.title = element_text(size = 60, hjust = 0.5), #图标题字体、居中
    plot.tag = element_text(size = 90, face = "bold") #设置标签文字大小
  ) +
  coord_fixed(ratio = 5/7) + #设置图形为长方形
  annotate(geom = "segment", x = 0, y = 0, xend = 1, yend = 1, color = "black", size = 0.2, linetype = "dotted") #添加对角线


##-----绘制测试队列校准曲线图-----
#提取测试队列任务数据
UH_test_task_data <- UH_test_task$data()
UH_test_task_data$CMDs <- factor(ifelse(UH_test_task_data$CMDs == "Yes", 1, 0))

#对模型进行评分
UH_test_score <- Score(list(LR = UH_test_prob_data[["LR"]],
                            RF = UH_test_prob_data[["RF"]],
                            XGBoost = UH_test_prob_data[["XGBoost"]],
                            SVM = UH_test_prob_data[["SVM"]],
                            KNN = UH_test_prob_data[["KNN"]],
                            DT = UH_test_prob_data[["DT"]],
                            ANN = UH_test_prob_data[["ANN"]]),
                       formula = CMDs ~ 1, #模型评估公式
                       null.model = F, #表示不使用空模型进行比较
                       plots = "calibration", #绘制校准曲线
                       data = UH_test_task_data)

#提取校准曲线数据
UH_test_calibration_plot <- plotCalibration(UH_test_score, method = "nne", bandwidth = 0.05, plot = FALSE)
UH_test_calibration_data <- imap_dfr(UH_test_calibration_plot$plotFrames, ~ {
  .x %>% 
    as_tibble() %>% 
    mutate(model = .y)  #.y是列表元素名（lr/rf/svm等）
})
UH_test_calibration_data$model <- factor(UH_test_calibration_data$model, levels = c("LR", "RF", "XGBoost", "SVM", "KNN", "DT", "ANN"))


#绘制校准曲线图
UH_test_calibration <- ggplot(UH_test_calibration_data, aes(x = Pred, y = Obs, color = model)) +
  geom_line(linewidth = 0.1) + #设置线的粗细
  #使用 geom_segment() 绘制完美校准线
  geom_segment(data = segment_data, aes(x = x, y = y, xend = xend, yend = yend, color = segment_type), linewidth = 0.2, linetype = "dotted") +
  scale_x_continuous(limits = c(0, 1),  #限制x轴刻度
                     breaks = seq(0, 1, by = 0.2), name = "Predicted Probability") + #设置x轴刻度和标签
  scale_y_continuous(limits = c(0, 1),  #限制y轴刻度
                     breaks = seq(0, 1, by = 0.2), name = "Actual Probability") + #设置y轴刻度和标签
  #自定义颜色和图例标签
  scale_color_manual(
    values = c("#4DAF9C", "#807DBA", "#E34A33", "#3182BD", "#A63603", "#31A354", "#8C6D31", "black"),
    labels = c("LR", "RF", "XGBoost", "SVM", "KNN", "DT", "ANN", "Ideal")
  ) +
  labs(title = "Calibration Curve", #图标题
       color = "") + #图例标题为空
  theme_minimal() + #设置图表的主题
  theme(
    plot.background = element_rect(fill = "white", color = NA), #整体图像背景，边框无颜色
    panel.grid = element_blank(), #去掉网格线
    panel.border = element_rect(color = "black", fill = NA), #添加边框
    axis.ticks.x = element_line(color = "black"), #x轴刻度线
    axis.ticks.y = element_line(color = "black"), #y轴刻度线
    axis.ticks.length = unit(0.1, "cm"), #刻度线在外侧及长度
    legend.position = c(0.15, 0.82), #图例放置左上角
    legend.key.height = unit(0.2, "cm"), #控制每个图例项的高度
    legend.key.width = unit(0.4, "cm"), #控制每个图例项的宽度
    axis.title = element_text(size = 60), #坐标轴标题字体大小
    axis.text = element_text(size = 50), #坐标轴刻度字体大小
    legend.text = element_text(size = 30), #图例字体大小
    plot.title = element_text(size = 60, hjust = 0.5), #图标题字体、居中
    plot.tag = element_text(size = 90, face = "bold") #设置标签文字大小
  ) +
  coord_fixed(ratio = 5/7) #设置图形为长方形


#-----绘制测试队列DCA图-----
UH_test_dca <- ggplot(UH_test_dca_data, aes(x = threshold, y = net_benefit, color = model)) +
  geom_line(linewidth = 0.1) + #设置线的粗细
  scale_x_continuous(limits = c(0, 0.8), #限制x轴刻度
                     breaks = seq(0, 0.8, by = 0.2), #设置x轴刻度
                     name = "High Risk Threshold") + #设置x轴标签
  scale_y_continuous(limits = c(-0.2, 0.8), #限制y轴刻度
                     breaks = seq(-0.2, 0.8, by = 0.2), #设置y轴刻度
                     name = "Standardized Net Benefit") + #设置y轴标签
  scale_color_manual(values = c("#4DAF9C", "#807DBA", "#E34A33", "#3182BD", 
                                "#A63603", "#31A354", "#8C6D31", "gray", "black")) + #自定义颜色
  labs(title = "DCA", #图标题
       color = "") + #图例标题为空
  theme_minimal() + #设置图表的主题
  theme(
    plot.background = element_rect(fill = "white", color = NA), #整体图像背景，边框无颜色
    panel.grid = element_blank(), #去掉网格线
    panel.border = element_rect(color = "black", fill = NA), #添加边框
    axis.ticks.x = element_line(color = "black"), #x轴刻度线
    axis.ticks.y = element_line(color = "black"), #y轴刻度线
    axis.ticks.length = unit(0.1, "cm"), #刻度线在外侧及长度
    legend.position = c(0.86, 0.79), #图例放置右上角
    legend.key.height = unit(0.2, "cm"), #控制每个图例项的高度
    legend.key.width = unit(0.4, "cm"), #控制每个图例项的宽度
    axis.title = element_text(size = 60), #坐标轴标题字体大小
    axis.text = element_text(size = 50), #坐标轴刻度字体大小
    legend.text = element_text(size = 30), #图例字体大小
    plot.title = element_text(size = 60, hjust = 0.5), #图标题字体、居中
    plot.tag = element_text(size = 90, face = "bold") #设置标签文字大小
  ) +
  coord_fixed(ratio = 4/7) #设置图形为长方形


##-----绘制测试队列雷达图-----
#将数据转换为长格式，适合绘制雷达图
UH_test_radar_data <- UH_test_metrics %>%
  select(3, 5:11) %>%
  mutate(across(ends_with("_CI"),
                ~ as.numeric(sub("\\s*\\(.*$", "", .x))))
UH_test_radar_data <- t(UH_test_radar_data)  #转置数据框
UH_test_radar_data <- as.data.frame(UH_test_radar_data)  #转换回数据框
colnames(UH_test_radar_data) <- c("LR", "RF", "XGBoost", "SVM", "KNN", "DT", "ANN") #添加列名
UH_test_radar_data$metrics <- c("AUC", "Sensitivity", "Specificity", "PPV", "NPV", "CCR", "F1 score", "Brier score") #添加性能列
UH_test_radar_data <- UH_test_radar_data[, c("metrics", "LR", "RF", "XGBoost", "SVM", "KNN", "DT", "ANN")] #排序
UH_test_radar_data$metrics <- factor(UH_test_radar_data$metrics, levels=c("AUC", "Sensitivity", "Specificity", "PPV", "NPV", "CCR", "F1 score", "Brier score")) #转换为因子
#绘制雷达图-AUC
UH_test_AUC <- ggradar(UH_test_radar_data[1,], 
                       grid.line.width = 0.4, #网格线宽度
                       grid.label.size = 11, #网格线标签字体大小
                       axis.label.size = 9, #轴标签字体大小
                       grid.min = 0, #网格线最小值
                       grid.mid = 0.45, #网格线均值
                       grid.max = 0.9, #网格线最大值
                       values.radar = c(0, 0.45, 0.9), #轴标签显示
                       background.circle.colour = "white", #背景颜色
                       group.line.width = 0.5, #线宽
                       group.point.size = 1, #数据点大小
                       fill = T, #填充色
                       fill.alpha = 0.3, #填充色不透明度
                       group.colours = "#95B0E0") +
  ggtitle("AUC") + #添加标题
  theme(plot.title = element_text(hjust = 0.5, size = 40), #hjust=0.5标题居中，设置标签文字大小
        plot.margin = margin(0, 0, 0, 0, "cm"), #调整边距
        plot.tag = element_text(size = 50, face = "bold") #右上角标签字体大小 
  ) +
  coord_fixed(ratio = 1) #设置图形为正方形

#绘制雷达图-Sensitivity
UH_test_Sensitivity <- ggradar(UH_test_radar_data[2,], 
                               grid.line.width = 0.4, #网格线宽度
                               grid.label.size = 11, #网格线标签字体大小
                               axis.label.size = 9, #轴标签字体大小
                               grid.min = 0, #网格线最小值
                               grid.mid = 0.4, #网格线均值
                               grid.max = 0.8, #网格线最大值
                               values.radar = c(0, 0.4, 0.8), #轴标签显示
                               background.circle.colour = "white", #背景颜色
                               group.line.width = 0.5, #线宽
                               group.point.size = 1, #数据点大小
                               fill = T, #填充色
                               fill.alpha = 0.3, #填充色不透明度
                               group.colours = "#56AEDE") +
  ggtitle("Sensitivity") +#添加标题
  theme(plot.title = element_text(hjust = 0.5, size = 40), #hjust=0.5标题居中，设置标签文字大小
        plot.margin = margin(0, 0, 0, 0, "cm"), #调整边距
        plot.tag = element_text(size = 50, face = "bold") #右上角标签字体大小 
  ) +
  coord_fixed(ratio = 1) #设置图形为正方形

#绘制雷达图-Specificity
UH_test_Specificity <- ggradar(UH_test_radar_data[3,], 
                               grid.line.width = 0.4, #网格线宽度
                               grid.label.size = 11, #网格线标签字体大小
                               axis.label.size = 9, #轴标签字体大小
                               grid.min = 0, #网格线最小值
                               grid.mid = 0.5, #网格线均值
                               grid.max = 1, #网格线最大值
                               values.radar = c(0, 0.5, 1), #轴标签显示
                               background.circle.colour = "white", #背景颜色
                               group.line.width = 0.5, #线宽
                               group.point.size = 1, #数据点大小
                               fill = T, #填充色
                               fill.alpha = 0.2, #填充色不透明度
                               group.colours = "#EE7A5F") +
  ggtitle("Specificity") + #添加标题
  theme(plot.title = element_text(hjust = 0.5, size = 40), #hjust=0.5标题居中，设置标签文字大小
        plot.margin = margin(0, 0, 0, 0, "cm"), #调整边距
        plot.tag = element_text(size = 50, face = "bold") #右上角标签字体大小 
  ) +
  coord_fixed(ratio = 1) #设置图形为正方形

#绘制雷达图-PPV
UH_test_PPV <- ggradar(UH_test_radar_data[4,], 
                       grid.line.width = 0.4, #网格线宽度
                       grid.label.size = 11, #网格线标签字体大小
                       axis.label.size = 9, #轴标签字体大小
                       grid.min = 0, #网格线最小值
                       grid.mid = 0.3, #网格线均值
                       grid.max = 0.6, #网格线最大值
                       values.radar = c(0, 0.3, 0.6), #轴标签显示
                       background.circle.colour = "white", #背景颜色
                       group.line.width = 0.5, #线宽
                       group.point.size = 1, #数据点大小
                       fill = T, #填充色
                       fill.alpha = 0.2, #填充色不透明度
                       group.colours = "#FEAF8A") +
  ggtitle("PPV") + #添加标题
  theme(plot.title = element_text(hjust = 0.5, size = 40), #hjust=0.5标题居中，设置标签文字大小
        plot.margin = margin(0, 0, 0, 0, "cm"), #调整边距
        plot.tag = element_text(size = 50, face = "bold") #右上角标签字体大小 
  ) +
  coord_fixed(ratio = 1) #设置图形为正方形

#绘制雷达图-NPV
UH_test_NPV <- ggradar(UH_test_radar_data[5,], 
                       grid.line.width = 0.4, #网格线宽度
                       grid.label.size = 11, #网格线标签字体大小
                       axis.label.size = 9, #轴标签字体大小
                       grid.min = 0, #网格线最小值
                       grid.mid = 0.5, #网格线均值
                       grid.max = 1, #网格线最大值
                       values.radar = c(0, 0.5, 1), #轴标签显示
                       background.circle.colour = "white", #背景颜色
                       group.line.width = 0.5, #线宽
                       group.point.size = 1, #数据点大小
                       fill = T, #填充色
                       fill.alpha = 0.3, #填充色不透明度
                       group.colours = "#F6B7C6") +
  ggtitle("NPV") + #添加标题
  theme(plot.title = element_text(hjust = 0.5, size = 40), #hjust=0.5标题居中，设置标签文字大小
        plot.margin = margin(0, 0, 0, 0, "cm"), #调整边距
        plot.tag = element_text(size = 50, face = "bold") #右上角标签字体大小 
  ) +
  coord_fixed(ratio = 1) #设置图形为正方形

#绘制雷达图-CCR
UH_test_CCR <- ggradar(UH_test_radar_data[6,], 
                       grid.line.width = 0.4, #网格线宽度
                       grid.label.size = 11, #网格线标签字体大小
                       axis.label.size = 9, #轴标签字体大小
                       grid.min = 0, #网格线最小值
                       grid.mid = 0.5, #网格线均值
                       grid.max = 1, #网格线最大值
                       values.radar = c(0, 0.5, 1), #轴标签显示
                       background.circle.colour = "white", #背景颜色
                       group.line.width = 0.5, #线宽
                       group.point.size = 1, #数据点大小
                       fill = T, #填充色
                       fill.alpha = 0.3, #填充色不透明度
                       group.colours = "#D8CBF0") +
  ggtitle("CCR") + #添加标题
  theme(plot.title = element_text(hjust = 0.5, size = 40), #hjust=0.5标题居中，设置标签文字大小
        plot.margin = margin(0, 0, 0, 0, "cm"), #调整边距
        plot.tag = element_text(size = 50, face = "bold") #右上角标签字体大小 
  ) +
  coord_fixed(ratio = 1) #设置图形为正方形

#绘制雷达图-F1 score
UH_test_F1_score <- ggradar(UH_test_radar_data[7,], 
                            grid.line.width = 0.4, #网格线宽度
                            grid.label.size = 11, #网格线标签字体大小
                            axis.label.size = 9, #轴标签字体大小
                            grid.min = 0, #网格线最小值
                            grid.mid = 0.25, #网格线均值
                            grid.max = 0.5, #网格线最大值
                            values.radar = c(0, 0.25, 0.5), #轴标签显示
                            background.circle.colour = "white", #背景颜色
                            group.line.width = 0.5, #线宽
                            group.point.size = 1, #数据点大小
                            fill = T, #填充色
                            fill.alpha = 0.3, #填充色不透明度
                            group.colours = "#9FCDC9") +
  ggtitle("F1 score") + #添加标题
  theme(plot.title = element_text(hjust = 0.5, size = 40), #hjust=0.5标题居中，设置标签文字大小
        plot.margin = margin(0, 0, 0, 0, "cm"), #调整边距
        plot.tag = element_text(size = 50, face = "bold") #右上角标签字体大小 
  ) +
  coord_fixed(ratio = 1) #设置图形为正方形

#绘制雷达图-Brier score
UH_test_Brier_score <- ggradar(UH_test_radar_data[8,], 
                               grid.line.width = 0.4, #网格线宽度
                               grid.label.size = 11, #网格线标签字体大小
                               axis.label.size = 9, #轴标签字体大小
                               grid.min = 0, #网格线最小值
                               grid.mid = 0.05, #网格线均值
                               grid.max = 0.1, #网格线最大值
                               values.radar = c(0, 0.05, 0.1), #轴标签显示
                               background.circle.colour = "white", #背景颜色
                               group.line.width = 0.5, #线宽
                               group.point.size = 1, #数据点大小
                               fill = T, #填充色
                               fill.alpha = 0.2, #填充色不透明度
                               group.colours = "#7EB87B") +
  ggtitle("Brier score") + #添加标题
  theme(plot.title = element_text(hjust = 0.5, size = 40), #hjust=0.5标题居中，设置标签文字大小
        plot.margin = margin(0, 0, 0, 0, "cm"), #调整边距
        plot.tag = element_text(size = 50, face = "bold") #右上角标签字体大小 
  ) +
  coord_fixed(ratio = 1) #设置图形为正方形

#合并测试队列各性能雷达图
UH_test_radar <- (UH_test_AUC + UH_test_Sensitivity + UH_test_Specificity + UH_test_PPV + 
                    UH_test_NPV + UH_test_CCR + UH_test_F1_score + UH_test_Brier_score) + 
  plot_annotation(tag_levels = "A") + #自动添加A和B标签
  plot_layout(ncol = 4, nrow = 2) #图片2*4

#导出测试队列各性能雷达图
ggsave(plot = UH_test_radar, 
       filename = "UH_test_radar.png", 
       width = 17, 
       height = 8.5,
       units = "cm", 
       dpi = 600)


#合并两组的模型性能结果
test_metrics_37 <- rbind(LH_test_metrics, UH_test_metrics)

#合并两组的delong test结果
test_roc_comparisons_37 <- merge(LH_test_auc_comparisons, UH_test_auc_comparisons, 
                                 by = "Model_comparison")


#合并两组ROC图、校准曲线图、DCA图
ROC_calibration_dca_37 <- (LH_test_roc + UH_test_roc) / 
  (LH_test_calibration + UH_test_calibration) / 
  (LH_test_dca + UH_test_dca) + 
  plot_annotation(tag_levels = "A") #自动添加A和B标签

#导出两组ROC图、校准曲线图、DCA图
ggsave(plot = ROC_calibration_dca_37, 
       filename = "ROC_calibration_dca_37.png", 
       width = 16, 
       height = 20,
       units = "cm", 
       dpi = 600)

##合并两组各性能雷达图
#只给两组图加标签
LH_test_AUC_tag <- LH_test_AUC +
  labs(tag = "A") +
  theme(plot.tag.position = c(0.02, 1),
        plot.tag = element_text(size = 60, face = "bold"))

UH_test_AUC_tag <- UH_test_AUC +
  labs(tag = "B") +
  theme(plot.tag.position = c(0.02, 1),
        plot.tag = element_text(size = 60, face = "bold"))

LH_UH_test_radar <- (
  LH_test_AUC_tag + LH_test_Sensitivity + LH_test_Specificity + LH_test_PPV +
    LH_test_NPV + LH_test_CCR + LH_test_F1_score + LH_test_Brier_score +
    UH_test_AUC_tag + UH_test_Sensitivity + UH_test_Specificity + UH_test_PPV +
    UH_test_NPV + UH_test_CCR + UH_test_F1_score + UH_test_Brier_score
) +
  plot_layout(ncol = 4, nrow = 4)

#导出测试队列各性能雷达图
ggsave(plot = LH_UH_test_radar, 
       filename = "LH_UH_test_radar.png", 
       width = 17, 
       height = 15,
       units = "cm", 
       dpi = 600)


###------------------特征选择：RFE+SHAP------------------
##-----特征选择：Low decline和High-stable组-----
# 初始特征（从原始数据列名得到，避免 task 名称搞混）
current_feats <- setdiff(colnames(data_LH_train_std_num), "CMDs")

#创建输出列表
LH_round_outputs <- list()

#轮次计数
iter <- 1

#创建函数：给定特征集合 → 建 task → 调参 → 训练 → 预测 → 算 SHAP
LH_task_pred_shap <- function(train_df, test_df, feats) {
  # 1) 用“剔除后的数据集”创建任务
  train_task <- as_task_classif(train_df[, c("CMDs", feats), drop = FALSE], target = "CMDs")
  test_task <- as_task_classif(test_df[,  c("CMDs", feats), drop = FALSE], target = "CMDs")
  
  # 2) 训练模型和预测
  ##XGBoost
  xgb_learner <- lrn("classif.xgboost", predict_type = "prob")
  xgb_learner$param_set$values <- list(
    nrounds = to_tune(500, 1000), #树的数量：控制迭代次数，决定模型学习的程度。
    max_depth = to_tune(1, 10), #树的最大深度：控制树的复杂度，防止过拟合。
    eta = to_tune(0.001, 0.05), #学习率：控制学习的速度，影响收敛和稳定性。
    min_child_weight = to_tune(1, 10), #每个叶节点最小实例权重：限制分裂，防止过拟合。
    subsample = to_tune(0.1, 0.5) #子样本比例：提供随机性，减少过拟合风险，增加多样性。
  )
  set.seed(123)
  xgb <- tune(tuner = tnr("grid_search", resolution = 5),
              task = train_task,
              learner = xgb_learner,
              resampling = rsmp("cv", folds = 5),
              measure = msr("classif.auc")
  )
  xgb_learner$param_set$values <- xgb$result_learner_param_vals
  xgb_learner$train(train_task)
  train_xgb_pred <- xgb_learner$predict(train_task)
  test_xgb_pred <- xgb_learner$predict(test_task)
  
  # 3) 计算 SHAP
  #提取训练队列和测试队列预测变量
  train_x <- train_task$data(cols = train_task$feature_names)
  test_x <- test_task$data(cols = test_task$feature_names)
  #随机抽取训练队列200行
  set.seed(123)
  train_x_200 <- train_x %>% 
    slice_sample(n = 200)
  #计算SHAP值：svm_learner训练好的模型；X = test_x需要解释的测试数据集；bg_X = train_x_200：背景数据集，用于计算 SHAP 值的基准；predict_type = "prob"：指定模型的预测类型为概率；verbose = F关闭详细输出。
  shap_value <- kernelshap(xgb_learner, X = test_x, bg_X = train_x_200, predict_type = "prob", verbose = T)
  
  #对象输出为列表
  list(
    xgb_learner    = xgb_learner,
    train_xgb_pred = train_xgb_pred,
    test_xgb_pred  = test_xgb_pred,
    shap_value     = shap_value
  )
}

#while 循环：每轮做一次“训练+SHAP+删特征”，直到只剩很少特征
while (length(current_feats) > 4) {
  #本轮基于当前特征：先同步“剔除后数据集”
  train_df_i <- data_LH_train_std_num[, c("CMDs", current_feats), drop = FALSE]
  test_df_i  <- data_LH_test_std_num[,  c("CMDs", current_feats), drop = FALSE]
  
  #训练/预测/SHAP（都基于本轮剔除后的数据集）
  out <- LH_task_pred_shap(train_df_i, test_df_i, current_feats)
  
  #保存本轮的 4 个对象（并存一下当前特征，方便追溯）
  LH_round_outputs[[iter]] <- list(
    iter = iter,
    features = current_feats,
    xgb_learner    = out$xgb_learner,
    train_xgb_pred = out$train_xgb_pred,
    test_xgb_pred  = out$test_xgb_pred,
    shap_value     = out$shap_value
  )
  
  ###用 SHAP 排名，决定剔除末尾 5 个特征
  ##计算每个特征平均SHAP值
  # 1) 每个特征的 mean(|SHAP|)
  mean_abs_shap <- colMeans(abs(out$shap_value[["S"]][["Yes"]]), na.rm = TRUE)
  # 2) 排序（从重要到不重要）
  mean_abs_shap_sorted <- sort(mean_abs_shap, decreasing = TRUE)
  
  # 更新特征集合（下一轮会在两个数据集里同步剔除这些列）
  drop_n <- min(5, length(current_feats) - 5)
  
  #如果 drop_n <= 0，用 break 退出 while 循环
  if (drop_n <= 0) break 
  
  #提取本轮要删除的特征名
  drop_feats <- tail(names(mean_abs_shap_sorted), drop_n)
  
  #把要删除的特征 drop_feats 移除，得到下一轮要用的特征集合
  current_feats <- setdiff(current_feats, drop_feats)
  
  #迭代轮次+1
  iter <- iter + 1
}


##-----特征选择：Unstable和High-stable组-----
# 初始特征（从原始数据列名得到，避免 task 名称搞混）
current_feats <- setdiff(colnames(data_LH_train_std_num), "CMDs")

#创建输出列表
UH_round_outputs <- list()

#轮次计数
iter <- 1

#创建函数：给定特征集合 → 建 task → 调参 → 训练 → 预测 → 算 SHAP
UH_task_pred_shap <- function(train_df, test_df, feats) {
  # 1) 用“剔除后的数据集”创建任务
  train_task <- as_task_classif(train_df[, c("CMDs", feats), drop = FALSE], target = "CMDs")
  test_task <- as_task_classif(test_df[,  c("CMDs", feats), drop = FALSE], target = "CMDs")
  
  # 2) 训练模型和预测
  ##XGBoost
  xgb_learner <- lrn("classif.xgboost", predict_type = "prob")
  xgb_learner$param_set$values <- list(
    nrounds = to_tune(100, 500), #树的数量：控制迭代次数，决定模型学习的程度。
    max_depth = to_tune(1, 10), #树的最大深度：控制树的复杂度，防止过拟合。
    eta = to_tune(0.001, 0.05), #学习率：控制学习的速度，影响收敛和稳定性。
    min_child_weight = to_tune(1, 10), #每个叶节点最小实例权重：限制分裂，防止过拟合。
    subsample = to_tune(0.5, 1) #子样本比例：提供随机性，减少过拟合风险，增加多样性。
  )
  set.seed(123)
  xgb <- tune(tuner = tnr("grid_search", resolution = 5),
              task = train_task,
              learner = xgb_learner,
              resampling = rsmp("cv", folds = 5),
              measure = msr("classif.auc")
  )
  xgb_learner$param_set$values <- xgb$result_learner_param_vals
  xgb_learner$train(train_task)
  train_xgb_pred <- xgb_learner$predict(train_task)
  test_xgb_pred <- xgb_learner$predict(test_task)
  
  # 3) 计算 SHAP
  #提取训练队列和测试队列预测变量
  train_x <- train_task$data(cols = train_task$feature_names)
  test_x <- test_task$data(cols = test_task$feature_names)
  #随机抽取训练队列200行
  set.seed(123)
  train_x_200 <- train_x %>% 
    slice_sample(n = 200)
  #计算SHAP值：svm_learner训练好的模型；X = test_x需要解释的测试数据集；bg_X = train_x_200：背景数据集，用于计算 SHAP 值的基准；predict_type = "prob"：指定模型的预测类型为概率；verbose = F关闭详细输出。
  shap_value <- kernelshap(xgb_learner, X = test_x, bg_X = train_x_200, predict_type = "prob", verbose = T)
  
  #对象输出为列表
  list(
    xgb_learner    = xgb_learner,
    train_xgb_pred = train_xgb_pred,
    test_xgb_pred  = test_xgb_pred,
    shap_value     = shap_value
  )
}

#while 循环：每轮做一次“训练+SHAP+删特征”，直到只剩很少特征
while (length(current_feats) > 4) {
  #本轮基于当前特征：先同步“剔除后数据集”
  train_df_i <- data_UH_train_std_num[, c("CMDs", current_feats), drop = FALSE]
  test_df_i  <- data_UH_test_std_num[,  c("CMDs", current_feats), drop = FALSE]
  
  #训练/预测/SHAP（都基于本轮剔除后的数据集）
  out <- UH_task_pred_shap(train_df_i, test_df_i, current_feats)
  
  #保存本轮的 4 个对象（并存一下当前特征，方便追溯）
  UH_round_outputs[[iter]] <- list(
    iter = iter,
    features = current_feats,
    xgb_learner    = out$xgb_learner,
    train_xgb_pred = out$train_xgb_pred,
    test_xgb_pred  = out$test_xgb_pred,
    shap_value     = out$shap_value
  )
  
  ###用 SHAP 排名，决定剔除末尾 5 个特征
  ##计算每个特征平均SHAP值
  # 1) 每个特征的 mean(|SHAP|)
  mean_abs_shap <- colMeans(abs(out$shap_value[["S"]][["Yes"]]), na.rm = TRUE)
  # 2) 排序（从重要到不重要）
  mean_abs_shap_sorted <- sort(mean_abs_shap, decreasing = TRUE)
  
  # 更新特征集合（下一轮会在两个数据集里同步剔除这些列）
  drop_n <- min(5, length(current_feats) - 5)
  
  #如果 drop_n <= 0，用 break 退出 while 循环
  if (drop_n <= 0) break 
  
  #提取本轮要删除的特征名
  drop_feats <- tail(names(mean_abs_shap_sorted), drop_n)
  
  #把要删除的特征 drop_feats 移除，得到下一轮要用的特征集合
  current_feats <- setdiff(current_feats, drop_feats)
  
  #迭代轮次+1
  iter <- iter + 1
}


###-----------不同数量风险因子性能评估-----------
##-----不同数量风险因子性能评估：Low decline和High-stable组-----
#创建训练队列和测试队列的模型预测结果列表
LH_train_preds_xgb <- list(
  xgb_35 = LH_round_outputs[[1]][["train_xgb_pred"]], 
  xgb_30 = LH_round_outputs[[2]][["train_xgb_pred"]], 
  xgb_25 = LH_round_outputs[[3]][["train_xgb_pred"]], 
  xgb_20 = LH_round_outputs[[4]][["train_xgb_pred"]], 
  xgb_15 = LH_round_outputs[[5]][["train_xgb_pred"]], 
  xgb_10 = LH_round_outputs[[6]][["train_xgb_pred"]], 
  xgb_5 = LH_round_outputs[[7]][["train_xgb_pred"]]
)
LH_test_preds_xgb <- list(
  xgb_35 = LH_round_outputs[[1]][["test_xgb_pred"]], 
  xgb_30 = LH_round_outputs[[2]][["test_xgb_pred"]], 
  xgb_25 = LH_round_outputs[[3]][["test_xgb_pred"]], 
  xgb_20 = LH_round_outputs[[4]][["test_xgb_pred"]], 
  xgb_15 = LH_round_outputs[[5]][["test_xgb_pred"]], 
  xgb_10 = LH_round_outputs[[6]][["test_xgb_pred"]], 
  xgb_5 = LH_round_outputs[[7]][["test_xgb_pred"]]
)

#创建空的数据框，分别用于存储训练队列和测试队列的性能评估结果
LH_train_metrics_xgb <- data.frame()
LH_test_metrics_xgb <- data.frame()

#创建空的数据框，分别用于存储绘制训练队列和测试队列的ROC数据
LH_train_roc_data_xgb <- data.frame()
LH_test_roc_data_xgb <- data.frame()

#创建列表来存储训练队列和测试队列的ROC()结果，后面进行AUC的DeLong检验
LH_train_roc_auc_xgb <- list()
LH_test_roc_auc_xgb <- list()

#创建列表来存储训练队列和测试队列模型的预测概率数据（校准曲线）
LH_train_prob_data_xgb <- list()
LH_test_prob_data_xgb <- list()

#创建空的数据框，分别用于存储训练队列和测试队列的dca数据
LH_train_dca_data_xgb <- data.frame()
LH_test_dca_data_xgb <- data.frame()


#计算训练队列的性能指标
for (model_name in names(LH_train_preds_xgb)) {
  #获取训练队列模型的预测结果
  pred_train <- LH_train_preds_xgb[[model_name]]
  #获取训练队列模型的预测概率
  pred_prob_train <- pred_train$prob[, "Yes"]
  
  #存训练队列模型的预测概率到列表
  LH_train_prob_data_xgb[[model_name]] <- pred_prob_train
  
  #获取训练队列的实际分类
  true_class_train <- pred_train$truth
  true_class_train <- ifelse(true_class_train == "Yes", 1, 0)
  
  roc_train <- roc(true_class_train, pred_prob_train)
  auc_train <- roc_train$auc
  ci_train <- ci(roc_train)
  #合并AUC和95%CI，保留三位小数
  auc_with_ci_train <- sprintf("%.3f\n(%.3f-%.3f)", auc_train, ci_train[1], ci_train[3]) 
  
  #存储roc()结果到列表
  LH_train_roc_auc_xgb[[model_name]] <- roc_train
  
  #提取绘制ROC曲线的数据并添加到数据框中
  roc_data_train <- data.frame(
    FPR = 1-roc_train$specificities, #假阳性率FPR: 1-specificities
    TPR = roc_train$sensitivities, #真阳性率TPR: sensitivities
    model = model_name #模型名称
  )
  #模型名排序
  roc_data_train$model <- factor(roc_data_train$model, levels = c("xgb_35", "xgb_30", "xgb_25", "xgb_20", "xgb_15", "xgb_10", "xgb_5"))
  #将测试队列各个模型的ROC数据合并
  LH_train_roc_data_xgb <- rbind(LH_train_roc_data, roc_data_train)
  
  ##计算训练队列的最佳阈值
  #定义阈值范围
  threshold <- seq(0, 1, by = 0.001)
  #对不同的阈值threshold计算性能指标
  metrics_list_train <- sapply(threshold, function(t) {
    calculate_metrics(pred_prob_train, true_class_train, t)
  }, simplify = F)
  
  distances <- sapply(metrics_list_train, function(metrics) {
    sqrt((1-metrics$Sensitivity)^2 + (1-metrics$Specificity)^2)
  })
  #找到训练队列的最佳阈值
  best_threshold_train <- threshold[which.min(distances)]
  
  #使用训练队列的最佳阈值计算训练队列的性能指标
  best_metrics_train <- calculate_metrics(pred_prob_train, true_class_train, best_threshold_train)
  
  #计算Brier score
  brier_score_train <- mean((pred_prob_train - true_class_train)^2)
  
  #Bootstrap CI
  set.seed(123)
  ci_train_metrics <- bootstrap_ci(pred_prob_train, true_class_train, best_threshold_train, B = 1000)
  
  Sensitivity_CI  <- format_ci(best_metrics_train$Sensitivity,  ci_train_metrics[, "Sensitivity"])
  Specificity_CI  <- format_ci(best_metrics_train$Specificity,  ci_train_metrics[, "Specificity"])
  PPV_CI          <- format_ci(best_metrics_train$PPV,          ci_train_metrics[, "PPV"])
  NPV_CI          <- format_ci(best_metrics_train$NPV,          ci_train_metrics[, "NPV"])
  CCR_CI          <- format_ci(best_metrics_train$CCR,          ci_train_metrics[, "CCR"])
  F1_score_CI     <- format_ci(best_metrics_train$F1_score,     ci_train_metrics[, "F1_score"])
  Brier_score_CI  <- format_ci(brier_score_train,               ci_train_metrics[, "Brier_score"])
  
  #汇总训练队列的模型结果
  train_metrics_result <- data.frame(
    Model = model_name,
    Dataset = "Training cohort",
    AUC_CI = auc_with_ci_train,
    Threshold = round(best_threshold_train, 3),
    Sensitivity_CI = Sensitivity_CI,
    Specificity_CI = Specificity_CI,
    PPV_CI = PPV_CI,
    NPV_CI = NPV_CI,
    CCR_CI = CCR_CI,
    F1_score_CI = F1_score_CI,
    Brier_score_CI = Brier_score_CI
  )
  #将训练队列各个模型的结果添加到数据框中
  LH_train_metrics_xgb <- rbind(LH_train_metrics_xgb, train_metrics_result)
  
  ##提取训练队列用于DCA数据
  #构造数据框：truth和预测概率
  train_truth_p <- data.frame(
    truth = true_class_train,  #将truth转换为0/1
    p = pred_prob_train  #提取预测概率
  )
  
  #计算决策曲线
  train_dca_result <- decision_curve(truth ~ p, data = train_truth_p, family = binomial, thresholds = seq(0, 1, by = 0.01))
  #提取数据并添加到数据框中
  train_thresholds <- train_dca_result$derived.data$thresholds
  train_net_benefit <- train_dca_result$derived.data$sNB
  train_model <- train_dca_result$derived.data$model
  train_model <- ifelse(train_model == "truth ~ p", model_name, train_model)
  #合并决策数据
  LH_train_dca_data_xgb <- rbind(LH_train_dca_data_xgb, data.frame(
    threshold = train_thresholds,
    net_benefit = train_net_benefit,
    model = train_model
  ))
  #模型名排序
  LH_train_dca_data_xgb$model <- factor(LH_train_dca_data_xgb$model, levels = c("xgb_35", "xgb_30", "xgb_25", "xgb_20", "xgb_15", "xgb_10", "xgb_5", "All", "None"))
  
}

#计算测试队列的性能指标
for (model_name in names(LH_test_preds_xgb)) {
  #获取测试队列模型的预测结果
  pred_test <- LH_test_preds_xgb[[model_name]]
  #获取测试队列模型的预测概率
  pred_prob_test <- pred_test$prob[, "Yes"]
  
  #存测试队列模型的预测概率到列表
  LH_test_prob_data_xgb[[model_name]] <- pred_prob_test
  
  #获取测试队列的实际分类
  true_class_test <- pred_test$truth
  true_class_test <- ifelse(true_class_test == "Yes", 1, 0)
  
  #计算测试队列AUC及95%CI
  roc_test <- roc(true_class_test, pred_prob_test)
  auc_test <- roc_test$auc
  ci_test <- ci(roc_test)
  #合并AUC和95%CI，保留三位小数
  auc_with_ci_test <- sprintf("%.3f\n(%.3f-%.3f)", auc_test, ci_test[1], ci_test[3]) 
  
  #存储roc()结果到列表
  LH_test_roc_auc_xgb[[model_name]] <- roc_test
  
  #提取绘制ROC曲线的数据并添加到数据框中
  roc_data_test <- data.frame(
    FPR = 1-roc_test$specificities, #假阳性率FPR: 1-specificities
    TPR = roc_test$sensitivities, #真阳性率TPR: sensitivities
    model = model_name #模型名称
  )
  #模型名排序
  roc_data_test$model <- factor(roc_data_test$model, levels = c("xgb_35", "xgb_30", "xgb_25", "xgb_20", "xgb_15", "xgb_10", "xgb_5"))
  #将测试队列各个模型的ROC数据合并
  LH_test_roc_data_xgb <- rbind(LH_test_roc_data_xgb, roc_data_test)
  
  #获取训练队列的最佳阈值
  best_threshold_train <- LH_train_metrics_xgb[LH_train_metrics_xgb$Model == model_name, "Threshold"]
  #使用训练队列的最佳阈值计算测试队列的性能
  best_metrics_test <- calculate_metrics(pred_prob_test, true_class_test, best_threshold_train)
  
  #计算测试队列的Brier score
  brier_score_test <- mean((pred_prob_test - true_class_test)^2)
  
  #Bootstrap CI
  set.seed(123)
  ci_test_metrics <- bootstrap_ci(pred_prob_test, true_class_test, best_threshold_train, B = 1000)
  
  Sensitivity_CI  <- format_ci(best_metrics_test$Sensitivity,  ci_test_metrics[, "Sensitivity"])
  Specificity_CI  <- format_ci(best_metrics_test$Specificity,  ci_test_metrics[, "Specificity"])
  PPV_CI          <- format_ci(best_metrics_test$PPV,          ci_test_metrics[, "PPV"])
  NPV_CI          <- format_ci(best_metrics_test$NPV,          ci_test_metrics[, "NPV"])
  CCR_CI          <- format_ci(best_metrics_test$CCR,          ci_test_metrics[, "CCR"])
  F1_score_CI     <- format_ci(best_metrics_test$F1_score,     ci_test_metrics[, "F1_score"])
  Brier_score_CI  <- format_ci(brier_score_test,               ci_test_metrics[, "Brier_score"])
  
  #汇总测试队列的模型结果到数据框中
  test_metrics_result <- data.frame(
    Model = model_name,
    Dataset = "Testing cohort",
    AUC_CI = auc_with_ci_test,
    Threshold = round(best_threshold_train, 3), #使用训练队列的最佳阈值
    Sensitivity_CI = Sensitivity_CI,
    Specificity_CI = Specificity_CI,
    PPV_CI = PPV_CI,
    NPV_CI = NPV_CI,
    CCR_CI = CCR_CI,
    F1_score_CI = F1_score_CI,
    Brier_score_CI = Brier_score_CI
  )
  #将测试队列各个模型的结果合并
  LH_test_metrics_xgb <- rbind(LH_test_metrics_xgb, test_metrics_result)
  
  ##提取测试队列用于DCA数据
  #构造数据框：truth和预测概率
  test_truth_p <- data.frame(
    truth = true_class_test,  #将truth转换为0/1
    p = pred_prob_test  #提取预测概率
  )
  
  #计算决策曲线
  test_dca_result <- decision_curve(truth ~ p, data = test_truth_p, family = binomial, thresholds = seq(0, 1, by = 0.01))
  #提取数据并添加到数据框中
  test_thresholds <- test_dca_result$derived.data$thresholds
  test_net_benefit <- test_dca_result$derived.data$sNB
  test_model <- test_dca_result$derived.data$model
  test_model <- ifelse(test_model == "truth ~ p", model_name, test_model)
  #合并决策数据
  LH_test_dca_data_xgb <- rbind(LH_test_dca_data_xgb, data.frame(
    threshold = test_thresholds,
    net_benefit = test_net_benefit,
    model = test_model
  ))
  #模型名排序
  LH_test_dca_data_xgb$model <- factor(LH_test_dca_data_xgb$model, levels = c("xgb_35", "xgb_30", "xgb_25", "xgb_20", "xgb_15", "xgb_10", "xgb_5", "All", "None"))
  
}


##delong test
#初始化一个空的数据框来存储测试队列的delong test结果
LH_test_auc_comparisons_xgb <- data.frame()
#获取训练队列模型名称的组合
model_names <- names(LH_test_roc_auc_xgb)

#两两比较ROC曲线，并提取测试队列的Z值和p-value
for (i in 1:(length(model_names) - 1)) {
  for (j in (i + 1):length(model_names)) {
    #获取当前两个模型的ROC曲线数据
    test_roc1 <- LH_test_roc_auc_xgb[[model_names[i]]]
    test_roc2 <- LH_test_roc_auc_xgb[[model_names[j]]]
    #DeLong检验
    test_delong_test <- roc.test(test_roc1, test_roc2, method = "delong")
    #提取Z值和p-value，分别保留三位小数和自定义函数
    test_Z_score <- round(test_delong_test$statistic, 3)
    test_P_value <- pvalue_format(test_delong_test$p.value)
    
    #将结果添加到数据框并合并
    LH_test_auc_comparisons_xgb <- rbind(LH_test_auc_comparisons_xgb, data.frame(
      Model_comparison = paste(model_names[i], model_names[j], sep = " vs. "),
      LH_test_Z_score = test_Z_score,
      LH_test_P_value = test_P_value
    ))
  }
}


#绘制测试集所有模型的ROC曲线
LH_test_roc_xgb <- ggplot(LH_test_roc_data_xgb, aes(x = FPR, y = TPR, color = model)) +
  geom_line(size = 0.1) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.2)) + # 设置x轴刻度
  scale_y_continuous(breaks = seq(0, 1, by = 0.2)) + # 设置y轴刻度
  #自定义颜色和图例标签
  scale_color_manual(
    values = c("#4DAF9C", "#807DBA", "#E34A33", "#3182BD", "#A63603", "#31A354", "#8C6D31"),
    labels = c(
      expression("35 features (AUC=0.806)"),
      expression("30 features (AUC=0.806, " * italic(P) * "=1.000)"),
      expression("25 features (AUC=0.806, " * italic(P) * "=1.000)"),
      expression("20 features (AUC=0.803, " * italic(P) * "=0.234)"),
      expression("15 features (AUC=0.807, " * italic(P) * "=0.484)"),
      expression("10 features (AUC=0.806, " * italic(P) * "=0.883)"),
      expression("5 features (AUC=0.805, " * italic(P) * "=0.867)"))
  ) + 
  labs(x = "1-Specificity",
       y = "Sensitivity",
       title = "ROC Curve", #图标题
       color = "") + #图例标题为空
  theme_minimal() + #设置图表的主题
  theme(
    plot.background = element_rect(fill = "white", color = NA), #整体图像背景，边框无颜色
    panel.grid = element_blank(), #去掉网格线
    panel.border = element_rect(color = "black", fill = NA), #添加边框
    axis.ticks.x = element_line(color = "black"), #x轴刻度线
    axis.ticks.y = element_line(color = "black"), #y轴刻度线
    axis.ticks.length = unit(0.1, "cm"), #刻度线在外侧及长度
    legend.position = c(0.75, 0.22), #图例放置右下角
    legend.key.height = unit(0.2, "cm"), #控制每个图例项的高度
    legend.key.width = unit(0.4, "cm"), #控制每个图例项的宽度
    axis.title = element_text(size = 60), #坐标轴标题字体大小
    axis.text = element_text(size = 50), #坐标轴刻度字体大小
    legend.text = element_text(size = 25), #图例字体大小
    plot.title = element_text(size = 60, hjust = 0.5), #图标题字体、居中
    plot.tag = element_text(size = 90, face = "bold") #设置标签文字大小
  ) +
  coord_fixed(ratio = 5/7) + #设置图形为长方形
  annotate(geom = "segment", x = 0, y = 0, xend = 1, yend = 1, color = "black", size = 0.2, linetype = "dotted") #添加对角线


##绘制测试队列校准曲线图
#提取测试队列任务数据
LH_test_task_data <- LH_test_task$data()
LH_test_task_data$CMDs <- factor(ifelse(LH_test_task_data$CMDs == "Yes", 1, 0))

#对模型进行评分
LH_test_score_xgb <- Score(list(xgb_35 = LH_test_prob_data_xgb[["xgb_35"]],
                                xgb_30 = LH_test_prob_data_xgb[["xgb_30"]],
                                xgb_25 = LH_test_prob_data_xgb[["xgb_25"]],
                                xgb_20 = LH_test_prob_data_xgb[["xgb_20"]],
                                xgb_15 = LH_test_prob_data_xgb[["xgb_15"]],
                                xgb_10 = LH_test_prob_data_xgb[["xgb_10"]],
                                xgb_5 = LH_test_prob_data_xgb[["xgb_5"]]),
                           formula = CMDs ~ 1, #模型评估公式
                           null.model = F, #表示不使用空模型进行比较
                           plots = "calibration", #绘制校准曲线
                           data = LH_test_task_data)

#提取校准曲线数据
LH_test_calibration_plot_xgb <- plotCalibration(LH_test_score_xgb, method = "nne", bandwidth = 0.05, plot = FALSE)
LH_test_calibration_data_xgb <- imap_dfr(LH_test_calibration_plot_xgb$plotFrames, ~ {
  .x %>% 
    as_tibble() %>% 
    mutate(model = .y)  #.y是列表元素名（lr/rf/svm等）
})
LH_test_calibration_data_xgb$model <- factor(LH_test_calibration_data_xgb$model, levels = c("xgb_35", "xgb_30", "xgb_25", "xgb_20", "xgb_15", "xgb_10", "xgb_5"))

#绘制校准曲线图
LH_test_calibration_xgb <- ggplot(LH_test_calibration_data_xgb, aes(x = Pred, y = Obs, color = model)) +
  geom_line(linewidth = 0.1) + #设置线的粗细
  #使用 geom_segment() 绘制完美校准线
  geom_segment(data = segment_data, aes(x = x, y = y, xend = xend, yend = yend, color = segment_type), linewidth = 0.2, linetype = "dotted") +
  scale_x_continuous(limits = c(0, 1),  #限制x轴刻度
                     breaks = seq(0, 1, by = 0.2), name = "Predicted Probability") + #设置x轴刻度和标签
  scale_y_continuous(limits = c(0, 1),  #限制y轴刻度
                     breaks = seq(0, 1, by = 0.2), name = "Actual Probability") + #设置y轴刻度和标签
  #自定义颜色和图例标签
  scale_color_manual(
    values = c("#4DAF9C", "#807DBA", "#E34A33", "#3182BD", "#A63603", "#31A354", "#8C6D31", "black"),
    labels = c("35 features", "30 features", "25 features","20 features", 
               "15 features", "10 features", "5 features", "Ideal")
  ) +
  labs(title = "Calibration Curve", #图标题
       color = "") + #图例标题为空
  theme_minimal() + #设置图表的主题
  theme(
    plot.background = element_rect(fill = "white", color = NA), #整体图像背景，边框无颜色
    panel.grid = element_blank(), #去掉网格线
    panel.border = element_rect(color = "black", fill = NA), #添加边框
    axis.ticks.x = element_line(color = "black"), #x轴刻度线
    axis.ticks.y = element_line(color = "black"), #y轴刻度线
    axis.ticks.length = unit(0.1, "cm"), #刻度线在外侧及长度
    legend.position = c(0.15, 0.82), #图例放置左上角
    legend.key.height = unit(0.2, "cm"), #控制每个图例项的高度
    legend.key.width = unit(0.4, "cm"), #控制每个图例项的宽度
    axis.title = element_text(size = 60), #坐标轴标题字体大小
    axis.text = element_text(size = 50), #坐标轴刻度字体大小
    legend.text = element_text(size = 30), #图例字体大小
    plot.title = element_text(size = 60, hjust = 0.5), #图标题字体、居中
    plot.tag = element_text(size = 90, face = "bold") #设置标签文字大小
  ) +
  coord_fixed(ratio = 5/7) #设置图形为长方形


#绘制测试队列DCA图
LH_test_dca_xgb <- ggplot(LH_test_dca_data_xgb, aes(x = threshold, y = net_benefit, color = model)) +
  geom_line(linewidth = 0.1) + #设置线的粗细
  scale_x_continuous(limits = c(0, 0.8), #限制x轴刻度
                     breaks = seq(0, 0.8, by = 0.2), #设置x轴刻度
                     name = "High Risk Threshold") + #设置x轴标签
  scale_y_continuous(limits = c(-0.2, 0.8), #限制y轴刻度
                     breaks = seq(-0.2, 0.8, by = 0.2), #设置y轴刻度
                     name = "Standardized Net Benefit") + #设置y轴标签
  scale_color_manual(values = c("#4DAF9C", "#807DBA", "#E34A33", "#3182BD", 
                                "#A63603", "#31A354", "#8C6D31", "gray", "black"), #自定义颜色
                     labels = c("35 features", "30 features", "25 features",
                                "20 features", "15 features", "10 features",
                                "5 features", "All", "None")
  ) +
  labs(title = "DCA", #图标题
       color = "") + #图例标题为空
  theme_minimal() + #设置图表的主题
  theme(
    plot.background = element_rect(fill = "white", color = NA), #整体图像背景，边框无颜色
    panel.grid = element_blank(), #去掉网格线
    panel.border = element_rect(color = "black", fill = NA), #添加边框
    axis.ticks.x = element_line(color = "black"), #x轴刻度线
    axis.ticks.y = element_line(color = "black"), #y轴刻度线
    axis.ticks.length = unit(0.1, "cm"), #刻度线在外侧及长度
    legend.position = c(0.86, 0.79), #图例放置右上角
    legend.key.height = unit(0.2, "cm"), #控制每个图例项的高度
    legend.key.width = unit(0.4, "cm"), #控制每个图例项的宽度
    axis.title = element_text(size = 60), #坐标轴标题字体大小
    axis.text = element_text(size = 50), #坐标轴刻度字体大小
    legend.text = element_text(size = 30), #图例字体大小
    plot.title = element_text(size = 60, hjust = 0.5), #图标题字体、居中
    plot.tag = element_text(size = 90, face = "bold") #设置标签文字大小
  ) +
  coord_fixed(ratio = 4/7) #设置图形为长方形


##绘制测试队列雷达图
#将数据转换为长格式，适合绘制雷达图
LH_test_radar_data_xgb <- LH_test_metrics_xgb %>%
  select(3, 5:11) %>%
  mutate(across(ends_with("_CI"),
                ~ as.numeric(sub("\\s*\\(.*$", "", .x))))
LH_test_radar_data_xgb <- t(LH_test_radar_data_xgb)  #转置数据框
LH_test_radar_data_xgb <- as.data.frame(LH_test_radar_data_xgb)  #转换回数据框
colnames(LH_test_radar_data_xgb) <- c("35", "30", "25", "20", "15", "10", "5") #添加列名
LH_test_radar_data_xgb$metrics <- c("AUC", "Sensitivity", "Specificity", "PPV", "NPV", "CCR", "F1 score", "Brier score") #添加性能列
LH_test_radar_data_xgb <- LH_test_radar_data_xgb[, c("metrics", "35", "30", "25", "20", "15", "10", "5")] #排序
LH_test_radar_data_xgb$metrics <- factor(LH_test_radar_data_xgb$metrics, levels=c("AUC", "Sensitivity", "Specificity", "PPV", "NPV", "CCR", "F1 score", "Brier score")) #转换为因子
#绘制雷达图-AUC
LH_test_AUC_xgb <- ggradar(LH_test_radar_data_xgb[1,], 
                           grid.line.width = 0.4, #网格线宽度
                           grid.label.size = 11, #网格线标签字体大小
                           axis.label.size = 9, #轴标签字体大小
                           grid.min = 0, #网格线最小值
                           grid.mid = 0.45, #网格线均值
                           grid.max = 0.9, #网格线最大值
                           values.radar = c(0, 0.45, 0.9), #轴标签显示
                           background.circle.colour = "white", #背景颜色
                           group.line.width = 0.5, #线宽
                           group.point.size = 1, #数据点大小
                           fill = T, #填充色
                           fill.alpha = 0.3, #填充色不透明度
                           group.colours = "#95B0E0") +
  ggtitle("AUC") + #添加标题
  theme(plot.title = element_text(hjust = 0.5, size = 40), #hjust=0.5标题居中，设置标签文字大小
        plot.margin = margin(0, 0, 0, 0, "cm"), #调整边距
        plot.tag = element_text(size = 50, face = "bold") #右上角标签字体大小 
  ) +
  coord_fixed(ratio = 1) #设置图形为正方形

#绘制雷达图-Sensitivity
LH_test_Sensitivity_xgb <- ggradar(LH_test_radar_data_xgb[2,], 
                                   grid.line.width = 0.4, #网格线宽度
                                   grid.label.size = 11, #网格线标签字体大小
                                   axis.label.size = 9, #轴标签字体大小
                                   grid.min = 0, #网格线最小值
                                   grid.mid = 0.4, #网格线均值
                                   grid.max = 0.8, #网格线最大值
                                   values.radar = c(0, 0.4, 0.8), #轴标签显示
                                   background.circle.colour = "white", #背景颜色
                                   group.line.width = 0.5, #线宽
                                   group.point.size = 1, #数据点大小
                                   fill = T, #填充色
                                   fill.alpha = 0.3, #填充色不透明度
                                   group.colours = "#56AEDE") +
  ggtitle("Sensitivity") +#添加标题
  theme(plot.title = element_text(hjust = 0.5, size = 40), #hjust=0.5标题居中，设置标签文字大小
        plot.margin = margin(0, 0, 0, 0, "cm"), #调整边距
        plot.tag = element_text(size = 50, face = "bold") #右上角标签字体大小 
  ) +
  coord_fixed(ratio = 1) #设置图形为正方形

#绘制雷达图-Specificity
LH_test_Specificity_xgb <- ggradar(LH_test_radar_data_xgb[3,], 
                                   grid.line.width = 0.4, #网格线宽度
                                   grid.label.size = 11, #网格线标签字体大小
                                   axis.label.size = 9, #轴标签字体大小
                                   grid.min = 0, #网格线最小值
                                   grid.mid = 0.4, #网格线均值
                                   grid.max = 0.8, #网格线最大值
                                   values.radar = c(0, 0.4, 0.8), #轴标签显示
                                   background.circle.colour = "white", #背景颜色
                                   group.line.width = 0.5, #线宽
                                   group.point.size = 1, #数据点大小
                                   fill = T, #填充色
                                   fill.alpha = 0.2, #填充色不透明度
                                   group.colours = "#EE7A5F") +
  ggtitle("Specificity") + #添加标题
  theme(plot.title = element_text(hjust = 0.5, size = 40), #hjust=0.5标题居中，设置标签文字大小
        plot.margin = margin(0, 0, 0, 0, "cm"), #调整边距
        plot.tag = element_text(size = 50, face = "bold") #右上角标签字体大小 
  ) +
  coord_fixed(ratio = 1) #设置图形为正方形

#绘制雷达图-PPV
LH_test_PPV_xgb <- ggradar(LH_test_radar_data_xgb[4,], 
                           grid.line.width = 0.4, #网格线宽度
                           grid.label.size = 11, #网格线标签字体大小
                           axis.label.size = 9, #轴标签字体大小
                           grid.min = 0, #网格线最小值
                           grid.mid = 0.15, #网格线均值
                           grid.max = 0.3, #网格线最大值
                           values.radar = c(0, 0.15, 0.3), #轴标签显示
                           background.circle.colour = "white", #背景颜色
                           group.line.width = 0.5, #线宽
                           group.point.size = 1, #数据点大小
                           fill = T, #填充色
                           fill.alpha = 0.2, #填充色不透明度
                           group.colours = "#FEAF8A") +
  ggtitle("PPV") + #添加标题
  theme(plot.title = element_text(hjust = 0.5, size = 40), #hjust=0.5标题居中，设置标签文字大小
        plot.margin = margin(0, 0, 0, 0, "cm"), #调整边距
        plot.tag = element_text(size = 50, face = "bold") #右上角标签字体大小 
  ) +
  coord_fixed(ratio = 1) #设置图形为正方形

#绘制雷达图-NPV
LH_test_NPV_xgb <- ggradar(LH_test_radar_data_xgb[5,], 
                           grid.line.width = 0.4, #网格线宽度
                           grid.label.size = 11, #网格线标签字体大小
                           axis.label.size = 9, #轴标签字体大小
                           grid.min = 0, #网格线最小值
                           grid.mid = 0.5, #网格线均值
                           grid.max = 1, #网格线最大值
                           values.radar = c(0, 0.5, 1), #轴标签显示
                           background.circle.colour = "white", #背景颜色
                           group.line.width = 0.5, #线宽
                           group.point.size = 1, #数据点大小
                           fill = T, #填充色
                           fill.alpha = 0.3, #填充色不透明度
                           group.colours = "#F6B7C6") +
  ggtitle("NPV") + #添加标题
  theme(plot.title = element_text(hjust = 0.5, size = 40), #hjust=0.5标题居中，设置标签文字大小
        plot.margin = margin(0, 0, 0, 0, "cm"), #调整边距
        plot.tag = element_text(size = 50, face = "bold") #右上角标签字体大小 
  ) +
  coord_fixed(ratio = 1) #设置图形为正方形

#绘制雷达图-CCR
LH_test_CCR_xgb <- ggradar(LH_test_radar_data_xgb[6,], 
                           grid.line.width = 0.4, #网格线宽度
                           grid.label.size = 11, #网格线标签字体大小
                           axis.label.size = 9, #轴标签字体大小
                           grid.min = 0, #网格线最小值
                           grid.mid = 0.4, #网格线均值
                           grid.max = 0.8, #网格线最大值
                           values.radar = c(0, 0.4, 0.8), #轴标签显示
                           background.circle.colour = "white", #背景颜色
                           group.line.width = 0.5, #线宽
                           group.point.size = 1, #数据点大小
                           fill = T, #填充色
                           fill.alpha = 0.3, #填充色不透明度
                           group.colours = "#D8CBF0") +
  ggtitle("CCR") + #添加标题
  theme(plot.title = element_text(hjust = 0.5, size = 40), #hjust=0.5标题居中，设置标签文字大小
        plot.margin = margin(0, 0, 0, 0, "cm"), #调整边距
        plot.tag = element_text(size = 50, face = "bold") #右上角标签字体大小 
  ) +
  coord_fixed(ratio = 1) #设置图形为正方形

#绘制雷达图-F1 score
LH_test_F1_score_xgb <- ggradar(LH_test_radar_data_xgb[7,], 
                                grid.line.width = 0.4, #网格线宽度
                                grid.label.size = 11, #网格线标签字体大小
                                axis.label.size = 9, #轴标签字体大小
                                grid.min = 0, #网格线最小值
                                grid.mid = 0.2, #网格线均值
                                grid.max = 0.4, #网格线最大值
                                values.radar = c(0, 0.2, 0.4), #轴标签显示
                                background.circle.colour = "white", #背景颜色
                                group.line.width = 0.5, #线宽
                                group.point.size = 1, #数据点大小
                                fill = T, #填充色
                                fill.alpha = 0.3, #填充色不透明度
                                group.colours = "#9FCDC9") +
  ggtitle("F1 score") + #添加标题
  theme(plot.title = element_text(hjust = 0.5, size = 40), #hjust=0.5标题居中，设置标签文字大小
        plot.margin = margin(0, 0, 0, 0, "cm"), #调整边距
        plot.tag = element_text(size = 50, face = "bold") #右上角标签字体大小 
  ) +
  coord_fixed(ratio = 1) #设置图形为正方形

#绘制雷达图-Brier score
LH_test_Brier_score_xgb <- ggradar(LH_test_radar_data_xgb[8,], 
                                   grid.line.width = 0.4, #网格线宽度
                                   grid.label.size = 11, #网格线标签字体大小
                                   axis.label.size = 9, #轴标签字体大小
                                   grid.min = 0, #网格线最小值
                                   grid.mid = 0.05, #网格线均值
                                   grid.max = 0.1, #网格线最大值
                                   values.radar = c(0, 0.05, 0.1), #轴标签显示
                                   background.circle.colour = "white", #背景颜色
                                   group.line.width = 0.5, #线宽
                                   group.point.size = 1, #数据点大小
                                   fill = T, #填充色
                                   fill.alpha = 0.2, #填充色不透明度
                                   group.colours = "#7EB87B") +
  ggtitle("Brier score") + #添加标题
  theme(plot.title = element_text(hjust = 0.5, size = 40), #hjust=0.5标题居中，设置标签文字大小
        plot.margin = margin(0, 0, 0, 0, "cm"), #调整边距
        plot.tag = element_text(size = 50, face = "bold") #右上角标签字体大小 
  ) +
  coord_fixed(ratio = 1) #设置图形为正方形

#合并测试队列各性能雷达图
LH_test_radar_xgb <- (LH_test_AUC_xgb + LH_test_Sensitivity_xgb + LH_test_Specificity_xgb + LH_test_PPV_xgb + 
                        LH_test_NPV_xgb + LH_test_CCR_xgb + LH_test_F1_score_xgb + LH_test_Brier_score_xgb) + 
  plot_annotation(tag_levels = "A") + #自动添加A和B标签
  plot_layout(ncol = 4, nrow = 2) #图片2*4

#导出测试队列各性能雷达图
ggsave(plot = LH_test_radar_xgb, 
       filename = "LH_test_radar_xgb.png", 
       width = 17, 
       height = 8.5,
       units = "cm", 
       dpi = 600)


##-----不同数量风险因子性能评估：Unstable和High-stable组-----
#创建训练队列和测试队列的模型预测结果列表
UH_train_preds_xgb <- list(
  xgb_35 = UH_round_outputs[[1]][["train_xgb_pred"]], 
  xgb_30 = UH_round_outputs[[2]][["train_xgb_pred"]], 
  xgb_25 = UH_round_outputs[[3]][["train_xgb_pred"]], 
  xgb_20 = UH_round_outputs[[4]][["train_xgb_pred"]], 
  xgb_15 = UH_round_outputs[[5]][["train_xgb_pred"]], 
  xgb_10 = UH_round_outputs[[6]][["train_xgb_pred"]], 
  xgb_5 = UH_round_outputs[[7]][["train_xgb_pred"]]
)
UH_test_preds_xgb <- list(
  xgb_35 = UH_round_outputs[[1]][["test_xgb_pred"]], 
  xgb_30 = UH_round_outputs[[2]][["test_xgb_pred"]], 
  xgb_25 = UH_round_outputs[[3]][["test_xgb_pred"]], 
  xgb_20 = UH_round_outputs[[4]][["test_xgb_pred"]], 
  xgb_15 = UH_round_outputs[[5]][["test_xgb_pred"]], 
  xgb_10 = UH_round_outputs[[6]][["test_xgb_pred"]], 
  xgb_5 = UH_round_outputs[[7]][["test_xgb_pred"]]
)

#创建空的数据框，分别用于存储训练队列和测试队列的性能评估结果
UH_train_metrics_xgb <- data.frame()
UH_test_metrics_xgb <- data.frame()

#创建空的数据框，分别用于存储绘制训练队列和测试队列的ROC数据
UH_train_roc_data_xgb <- data.frame()
UH_test_roc_data_xgb <- data.frame()

#创建列表来存储训练队列和测试队列的ROC()结果，后面进行AUC的DeLong检验
UH_train_roc_auc_xgb <- list()
UH_test_roc_auc_xgb <- list()

#创建列表来存储训练队列和测试队列模型的预测概率数据（校准曲线）
UH_train_prob_data_xgb <- list()
UH_test_prob_data_xgb <- list()

#创建空的数据框，分别用于存储训练队列和测试队列的dca数据
UH_train_dca_data_xgb <- data.frame()
UH_test_dca_data_xgb <- data.frame()


#计算训练队列的性能指标
for (model_name in names(UH_train_preds_xgb)) {
  #获取训练队列模型的预测结果
  pred_train <- UH_train_preds_xgb[[model_name]]
  #获取训练队列模型的预测概率
  pred_prob_train <- pred_train$prob[, "Yes"]
  
  #存训练队列模型的预测概率到列表
  UH_train_prob_data_xgb[[model_name]] <- pred_prob_train
  
  #获取训练队列的实际分类
  true_class_train <- pred_train$truth
  true_class_train <- ifelse(true_class_train == "Yes", 1, 0)
  
  roc_train <- roc(true_class_train, pred_prob_train)
  auc_train <- roc_train$auc
  ci_train <- ci(roc_train)
  #合并AUC和95%CI，保留三位小数
  auc_with_ci_train <- sprintf("%.3f\n(%.3f-%.3f)", auc_train, ci_train[1], ci_train[3]) 
  
  #存储roc()结果到列表
  UH_train_roc_auc_xgb[[model_name]] <- roc_train
  
  #提取绘制ROC曲线的数据并添加到数据框中
  roc_data_train <- data.frame(
    FPR = 1-roc_train$specificities, #假阳性率FPR: 1-specificities
    TPR = roc_train$sensitivities, #真阳性率TPR: sensitivities
    model = model_name #模型名称
  )
  #模型名排序
  roc_data_train$model <- factor(roc_data_train$model, levels = c("xgb_35", "xgb_30", "xgb_25", "xgb_20", "xgb_15", "xgb_10", "xgb_5"))
  #将测试队列各个模型的ROC数据合并
  UH_train_roc_data_xgb <- rbind(UH_train_roc_data_xgb, roc_data_train)
  
  ##计算训练队列的最佳阈值
  #定义阈值范围
  threshold <- seq(0, 1, by = 0.001)
  #对不同的阈值threshold计算性能指标
  metrics_list_train <- sapply(threshold, function(t) {
    calculate_metrics(pred_prob_train, true_class_train, t)
  }, simplify = F)
  
  distances <- sapply(metrics_list_train, function(metrics) {
    sqrt((1-metrics$Sensitivity)^2 + (1-metrics$Specificity)^2)
  })
  #找到训练队列的最佳阈值
  best_threshold_train <- threshold[which.min(distances)]
  
  #使用训练队列的最佳阈值计算训练队列的性能指标
  best_metrics_train <- calculate_metrics(pred_prob_train, true_class_train, best_threshold_train)
  
  #计算Brier score
  brier_score_train <- mean((pred_prob_train - true_class_train)^2)
  
  #Bootstrap CI
  set.seed(123)
  ci_train_metrics <- bootstrap_ci(pred_prob_train, true_class_train, best_threshold_train, B = 1000)
  
  Sensitivity_CI  <- format_ci(best_metrics_train$Sensitivity,  ci_train_metrics[, "Sensitivity"])
  Specificity_CI  <- format_ci(best_metrics_train$Specificity,  ci_train_metrics[, "Specificity"])
  PPV_CI          <- format_ci(best_metrics_train$PPV,          ci_train_metrics[, "PPV"])
  NPV_CI          <- format_ci(best_metrics_train$NPV,          ci_train_metrics[, "NPV"])
  CCR_CI          <- format_ci(best_metrics_train$CCR,          ci_train_metrics[, "CCR"])
  F1_score_CI     <- format_ci(best_metrics_train$F1_score,     ci_train_metrics[, "F1_score"])
  Brier_score_CI  <- format_ci(brier_score_train,               ci_train_metrics[, "Brier_score"])
  
  #汇总训练队列的模型结果
  train_metrics_result <- data.frame(
    Model = model_name,
    Dataset = "Training cohort",
    AUC_CI = auc_with_ci_train,
    Threshold = round(best_threshold_train, 3),
    Sensitivity_CI = Sensitivity_CI,
    Specificity_CI = Specificity_CI,
    PPV_CI = PPV_CI,
    NPV_CI = NPV_CI,
    CCR_CI = CCR_CI,
    F1_score_CI = F1_score_CI,
    Brier_score_CI = Brier_score_CI
  )
  #将训练队列各个模型的结果添加到数据框中
  UH_train_metrics_xgb <- rbind(UH_train_metrics_xgb, train_metrics_result)
  
  ##提取训练队列用于DCA数据
  #构造数据框：truth和预测概率
  train_truth_p <- data.frame(
    truth = true_class_train,  #将truth转换为0/1
    p = pred_prob_train  #提取预测概率
  )
  
  #计算决策曲线
  train_dca_result <- decision_curve(truth ~ p, data = train_truth_p, family = binomial, thresholds = seq(0, 1, by = 0.01))
  #提取数据并添加到数据框中
  train_thresholds <- train_dca_result$derived.data$thresholds
  train_net_benefit <- train_dca_result$derived.data$sNB
  train_model <- train_dca_result$derived.data$model
  train_model <- ifelse(train_model == "truth ~ p", model_name, train_model)
  #合并决策数据
  UH_train_dca_data_xgb <- rbind(UH_train_dca_data_xgb, data.frame(
    threshold = train_thresholds,
    net_benefit = train_net_benefit,
    model = train_model
  ))
  #模型名排序
  UH_train_dca_data_xgb$model <- factor(UH_train_dca_data_xgb$model, levels = c("xgb_35", "xgb_30", "xgb_25", "xgb_20", "xgb_15", "xgb_10", "xgb_5", "All", "None"))
  
}

#计算测试队列的性能指标
for (model_name in names(UH_test_preds_xgb)) {
  #获取测试队列模型的预测结果
  pred_test <- UH_test_preds_xgb[[model_name]]
  #获取测试队列模型的预测概率
  pred_prob_test <- pred_test$prob[, "Yes"]
  
  #存测试队列模型的预测概率到列表
  UH_test_prob_data_xgb[[model_name]] <- pred_prob_test
  
  #获取测试队列的实际分类
  true_class_test <- pred_test$truth
  true_class_test <- ifelse(true_class_test == "Yes", 1, 0)
  
  #计算测试队列AUC及95%CI
  roc_test <- roc(true_class_test, pred_prob_test)
  auc_test <- roc_test$auc
  ci_test <- ci(roc_test)
  #合并AUC和95%CI，保留三位小数
  auc_with_ci_test <- sprintf("%.3f\n(%.3f-%.3f)", auc_test, ci_test[1], ci_test[3]) 
  
  #存储roc()结果到列表
  UH_test_roc_auc_xgb[[model_name]] <- roc_test
  
  #提取绘制ROC曲线的数据并添加到数据框中
  roc_data_test <- data.frame(
    FPR = 1-roc_test$specificities, #假阳性率FPR: 1-specificities
    TPR = roc_test$sensitivities, #真阳性率TPR: sensitivities
    model = model_name #模型名称
  )
  #模型名排序
  roc_data_test$model <- factor(roc_data_test$model, levels = c("xgb_35", "xgb_30", "xgb_25", "xgb_20", "xgb_15", "xgb_10", "xgb_5"))
  #将测试队列各个模型的ROC数据合并
  UH_test_roc_data_xgb <- rbind(UH_test_roc_data_xgb, roc_data_test)
  
  #获取训练队列的最佳阈值
  best_threshold_train <- UH_train_metrics_xgb[UH_train_metrics_xgb$Model == model_name, "Threshold"]
  #使用训练队列的最佳阈值计算测试队列的性能
  best_metrics_test <- calculate_metrics(pred_prob_test, true_class_test, best_threshold_train)
  
  #计算测试队列的Brier score
  brier_score_test <- mean((pred_prob_test - true_class_test)^2)
  
  #Bootstrap CI
  set.seed(123)
  ci_test_metrics <- bootstrap_ci(pred_prob_test, true_class_test, best_threshold_train, B = 1000)
  
  Sensitivity_CI  <- format_ci(best_metrics_test$Sensitivity,  ci_test_metrics[, "Sensitivity"])
  Specificity_CI  <- format_ci(best_metrics_test$Specificity,  ci_test_metrics[, "Specificity"])
  PPV_CI          <- format_ci(best_metrics_test$PPV,          ci_test_metrics[, "PPV"])
  NPV_CI          <- format_ci(best_metrics_test$NPV,          ci_test_metrics[, "NPV"])
  CCR_CI          <- format_ci(best_metrics_test$CCR,          ci_test_metrics[, "CCR"])
  F1_score_CI     <- format_ci(best_metrics_test$F1_score,     ci_test_metrics[, "F1_score"])
  Brier_score_CI  <- format_ci(brier_score_test,               ci_test_metrics[, "Brier_score"])
  
  #汇总测试队列的模型结果到数据框中
  test_metrics_result <- data.frame(
    Model = model_name,
    Dataset = "Testing cohort",
    AUC_CI = auc_with_ci_test,
    Threshold = round(best_threshold_train, 3), #使用训练队列的最佳阈值
    Sensitivity_CI = Sensitivity_CI,
    Specificity_CI = Specificity_CI,
    PPV_CI = PPV_CI,
    NPV_CI = NPV_CI,
    CCR_CI = CCR_CI,
    F1_score_CI = F1_score_CI,
    Brier_score_CI = Brier_score_CI
  )
  #将测试队列各个模型的结果合并
  UH_test_metrics_xgb <- rbind(UH_test_metrics_xgb, test_metrics_result)
  
  ##提取测试队列用于DCA数据
  #构造数据框：truth和预测概率
  test_truth_p <- data.frame(
    truth = true_class_test,  #将truth转换为0/1
    p = pred_prob_test  #提取预测概率
  )
  
  #计算决策曲线
  test_dca_result <- decision_curve(truth ~ p, data = test_truth_p, family = binomial, thresholds = seq(0, 1, by = 0.01))
  #提取数据并添加到数据框中
  test_thresholds <- test_dca_result$derived.data$thresholds
  test_net_benefit <- test_dca_result$derived.data$sNB
  test_model <- test_dca_result$derived.data$model
  test_model <- ifelse(test_model == "truth ~ p", model_name, test_model)
  #合并决策数据
  UH_test_dca_data_xgb <- rbind(UH_test_dca_data_xgb, data.frame(
    threshold = test_thresholds,
    net_benefit = test_net_benefit,
    model = test_model
  ))
  #模型名排序
  UH_test_dca_data_xgb$model <- factor(UH_test_dca_data_xgb$model, levels = c("xgb_35", "xgb_30", "xgb_25", "xgb_20", "xgb_15", "xgb_10", "xgb_5", "All", "None"))
  
}


##delong test
#初始化一个空的数据框来存储测试队列的delong test结果
UH_test_auc_comparisons_xgb <- data.frame()
#获取训练队列模型名称的组合
model_names <- names(UH_test_roc_auc_xgb)

#两两比较ROC曲线，并提取测试队列的Z值和p-value
for (i in 1:(length(model_names) - 1)) {
  for (j in (i + 1):length(model_names)) {
    #获取当前两个模型的ROC曲线数据
    test_roc1 <- UH_test_roc_auc_xgb[[model_names[i]]]
    test_roc2 <- UH_test_roc_auc_xgb[[model_names[j]]]
    #DeLong检验
    test_delong_test <- roc.test(test_roc1, test_roc2, method = "delong")
    #提取Z值和p-value，分别保留三位小数和自定义函数
    test_Z_score <- round(test_delong_test$statistic, 3)
    test_P_value <- pvalue_format(test_delong_test$p.value)
    
    #将结果添加到数据框并合并
    UH_test_auc_comparisons_xgb <- rbind(UH_test_auc_comparisons_xgb, data.frame(
      Model_comparison = paste(model_names[i], model_names[j], sep = " vs. "),
      UH_test_Z_score = test_Z_score,
      UH_test_P_value = test_P_value
    ))
  }
}


#绘制测试集所有模型的ROC曲线
UH_test_roc_xgb <- ggplot(UH_test_roc_data_xgb, aes(x = FPR, y = TPR, color = model)) +
  geom_line(size = 0.1) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.2)) + # 设置x轴刻度
  scale_y_continuous(breaks = seq(0, 1, by = 0.2)) + # 设置y轴刻度
  #自定义颜色和图例标签
  scale_color_manual(
    values = c("#4DAF9C", "#807DBA", "#E34A33", "#3182BD", "#A63603", "#31A354", "#8C6D31"),
    labels = c(
      expression("35 features (AUC=0.805)"),
      expression("30 features (AUC=0.805, " * italic(P) * "=0.279)"),
      expression("25 features (AUC=0.805, " * italic(P) * "=0.429)"),
      expression("20 features (AUC=0.805, " * italic(P) * "=0.830)"),
      expression("15 features (AUC=0.802, " * italic(P) * "=0.360)"),
      expression("10 features (AUC=0.805, " * italic(P) * "=0.887)"),
      expression("5 features (AUC=0.802, " * italic(P) * "=0.617)"))
  ) + 
  labs(x = "1-Specificity",
       y = "Sensitivity",
       title = "ROC Curve", #图标题
       color = "") + #图例标题为空
  theme_minimal() + #设置图表的主题
  theme(
    plot.background = element_rect(fill = "white", color = NA), #整体图像背景，边框无颜色
    panel.grid = element_blank(), #去掉网格线
    panel.border = element_rect(color = "black", fill = NA), #添加边框
    axis.ticks.x = element_line(color = "black"), #x轴刻度线
    axis.ticks.y = element_line(color = "black"), #y轴刻度线
    axis.ticks.length = unit(0.1, "cm"), #刻度线在外侧及长度
    legend.position = c(0.75, 0.22), #图例放置右下角
    legend.key.height = unit(0.2, "cm"), #控制每个图例项的高度
    legend.key.width = unit(0.4, "cm"), #控制每个图例项的宽度
    axis.title = element_text(size = 60), #坐标轴标题字体大小
    axis.text = element_text(size = 50), #坐标轴刻度字体大小
    legend.text = element_text(size = 25), #图例字体大小
    plot.title = element_text(size = 60, hjust = 0.5), #图标题字体、居中
    plot.tag = element_text(size = 90, face = "bold") #设置标签文字大小
  ) +
  coord_fixed(ratio = 5/7) + #设置图形为长方形
  annotate(geom = "segment", x = 0, y = 0, xend = 1, yend = 1, color = "black", size = 0.2, linetype = "dotted") #添加对角线


##绘制测试队列校准曲线图
#对模型进行评分
UH_test_score_xgb <- Score(list(xgb_35 = UH_test_prob_data_xgb[["xgb_35"]],
                                xgb_30 = UH_test_prob_data_xgb[["xgb_30"]],
                                xgb_25 = UH_test_prob_data_xgb[["xgb_25"]],
                                xgb_20 = UH_test_prob_data_xgb[["xgb_20"]],
                                xgb_15 = UH_test_prob_data_xgb[["xgb_15"]],
                                xgb_10 = UH_test_prob_data_xgb[["xgb_10"]],
                                xgb_5 = UH_test_prob_data_xgb[["xgb_5"]]),
                           formula = CMDs ~ 1, #模型评估公式
                           null.model = F, #表示不使用空模型进行比较
                           plots = "calibration", #绘制校准曲线
                           data = UH_test_task_data)

#提取校准曲线数据
UH_test_calibration_plot_xgb <- plotCalibration(UH_test_score_xgb, method = "nne", bandwidth = 0.05, plot = FALSE)
UH_test_calibration_data_xgb <- imap_dfr(UH_test_calibration_plot_xgb$plotFrames, ~ {
  .x %>% 
    as_tibble() %>% 
    mutate(model = .y)  #.y是列表元素名（lr/rf/svm等）
})
UH_test_calibration_data_xgb$model <- factor(UH_test_calibration_data_xgb$model, levels = c("xgb_35", "xgb_30", "xgb_25", "xgb_20", "xgb_15", "xgb_10", "xgb_5"))

#绘制校准曲线图
UH_test_calibration_xgb <- ggplot(UH_test_calibration_data_xgb, aes(x = Pred, y = Obs, color = model)) +
  geom_line(linewidth = 0.1) + #设置线的粗细
  #使用 geom_segment() 绘制完美校准线
  geom_segment(data = segment_data, aes(x = x, y = y, xend = xend, yend = yend, color = segment_type), linewidth = 0.2, linetype = "dotted") +
  scale_x_continuous(limits = c(0, 1),  #限制x轴刻度
                     breaks = seq(0, 1, by = 0.2), name = "Predicted Probability") + #设置x轴刻度和标签
  scale_y_continuous(limits = c(0, 1),  #限制y轴刻度
                     breaks = seq(0, 1, by = 0.2), name = "Actual Probability") + #设置y轴刻度和标签
  #自定义颜色和图例标签
  scale_color_manual(
    values = c("#4DAF9C", "#807DBA", "#E34A33", "#3182BD", "#A63603", "#31A354", "#8C6D31", "black"),
    labels = c("35 features", "30 features", "25 features","20 features", 
               "15 features", "10 features", "5 features", "Ideal")
  ) +
  labs(title = "Calibration Curve", #图标题
       color = "") + #图例标题为空
  theme_minimal() + #设置图表的主题
  theme(
    plot.background = element_rect(fill = "white", color = NA), #整体图像背景，边框无颜色
    panel.grid = element_blank(), #去掉网格线
    panel.border = element_rect(color = "black", fill = NA), #添加边框
    axis.ticks.x = element_line(color = "black"), #x轴刻度线
    axis.ticks.y = element_line(color = "black"), #y轴刻度线
    axis.ticks.length = unit(0.1, "cm"), #刻度线在外侧及长度
    legend.position = c(0.15, 0.82), #图例放置左上角
    legend.key.height = unit(0.2, "cm"), #控制每个图例项的高度
    legend.key.width = unit(0.4, "cm"), #控制每个图例项的宽度
    axis.title = element_text(size = 60), #坐标轴标题字体大小
    axis.text = element_text(size = 50), #坐标轴刻度字体大小
    legend.text = element_text(size = 30), #图例字体大小
    plot.title = element_text(size = 60, hjust = 0.5), #图标题字体、居中
    plot.tag = element_text(size = 90, face = "bold") #设置标签文字大小
  ) +
  coord_fixed(ratio = 5/7) #设置图形为长方形


#绘制测试队列DCA图
UH_test_dca_xgb <- ggplot(UH_test_dca_data_xgb, aes(x = threshold, y = net_benefit, color = model)) +
  geom_line(linewidth = 0.1) + #设置线的粗细
  scale_x_continuous(limits = c(0, 0.8), #限制x轴刻度
                     breaks = seq(0, 0.8, by = 0.2), #设置x轴刻度
                     name = "High Risk Threshold") + #设置x轴标签
  scale_y_continuous(limits = c(-0.2, 0.8), #限制y轴刻度
                     breaks = seq(-0.2, 0.8, by = 0.2), #设置y轴刻度
                     name = "Standardized Net Benefit") + #设置y轴标签
  scale_color_manual(values = c("#4DAF9C", "#807DBA", "#E34A33", "#3182BD", 
                                "#A63603", "#31A354", "#8C6D31", "gray", "black"), #自定义颜色
                     labels = c("35 features", "30 features", "25 features",
                                "20 features", "15 features", "10 features",
                                "5 features", "All", "None")
  ) +
  labs(title = "DCA", #图标题
       color = "") + #图例标题为空
  theme_minimal() + #设置图表的主题
  theme(
    plot.background = element_rect(fill = "white", color = NA), #整体图像背景，边框无颜色
    panel.grid = element_blank(), #去掉网格线
    panel.border = element_rect(color = "black", fill = NA), #添加边框
    axis.ticks.x = element_line(color = "black"), #x轴刻度线
    axis.ticks.y = element_line(color = "black"), #y轴刻度线
    axis.ticks.length = unit(0.1, "cm"), #刻度线在外侧及长度
    legend.position = c(0.86, 0.79), #图例放置右上角
    legend.key.height = unit(0.2, "cm"), #控制每个图例项的高度
    legend.key.width = unit(0.4, "cm"), #控制每个图例项的宽度
    axis.title = element_text(size = 60), #坐标轴标题字体大小
    axis.text = element_text(size = 50), #坐标轴刻度字体大小
    legend.text = element_text(size = 30), #图例字体大小
    plot.title = element_text(size = 60, hjust = 0.5), #图标题字体、居中
    plot.tag = element_text(size = 90, face = "bold") #设置标签文字大小
  ) +
  coord_fixed(ratio = 4/7) #设置图形为正方形


##绘制测试队列雷达图
#将数据转换为长格式，适合绘制雷达图
UH_test_radar_data_xgb <- UH_test_metrics_xgb %>%
  select(3, 5:11) %>%
  mutate(across(ends_with("_CI"),
                ~ as.numeric(sub("\\s*\\(.*$", "", .x))))
UH_test_radar_data_xgb <- t(UH_test_radar_data_xgb)  #转置数据框
UH_test_radar_data_xgb <- as.data.frame(UH_test_radar_data_xgb)  #转换回数据框
colnames(UH_test_radar_data_xgb) <- c("35", "30", "25", "20", "15", "10", "5") #添加列名
UH_test_radar_data_xgb$metrics <- c("AUC", "Sensitivity", "Specificity", "PPV", "NPV", "CCR", "F1 score", "Brier score") #添加性能列
UH_test_radar_data_xgb <- UH_test_radar_data_xgb[, c("metrics", "35", "30", "25", "20", "15", "10", "5")] #排序
UH_test_radar_data_xgb$metrics <- factor(UH_test_radar_data_xgb$metrics, levels=c("AUC", "Sensitivity", "Specificity", "PPV", "NPV", "CCR", "F1 score", "Brier score")) #转换为因子
#绘制雷达图-AUC
UH_test_AUC_xgb <- ggradar(UH_test_radar_data_xgb[1,], 
                           grid.line.width = 0.4, #网格线宽度
                           grid.label.size = 11, #网格线标签字体大小
                           axis.label.size = 9, #轴标签字体大小
                           grid.min = 0, #网格线最小值
                           grid.mid = 0.45, #网格线均值
                           grid.max = 0.9, #网格线最大值
                           values.radar = c(0, 0.45, 0.9), #轴标签显示
                           background.circle.colour = "white", #背景颜色
                           group.line.width = 0.5, #线宽
                           group.point.size = 1, #数据点大小
                           fill = T, #填充色
                           fill.alpha = 0.3, #填充色不透明度
                           group.colours = "#95B0E0") +
  ggtitle("AUC") + #添加标题
  theme(plot.title = element_text(hjust = 0.5, size = 40), #hjust=0.5标题居中，设置标签文字大小
        plot.margin = margin(0, 0, 0, 0, "cm"), #调整边距
        plot.tag = element_text(size = 50, face = "bold") #右上角标签字体大小 
  ) +
  coord_fixed(ratio = 1) #设置图形为正方形

#绘制雷达图-Sensitivity
UH_test_Sensitivity_xgb <- ggradar(UH_test_radar_data_xgb[2,], 
                                   grid.line.width = 0.4, #网格线宽度
                                   grid.label.size = 11, #网格线标签字体大小
                                   axis.label.size = 9, #轴标签字体大小
                                   grid.min = 0, #网格线最小值
                                   grid.mid = 0.4, #网格线均值
                                   grid.max = 0.8, #网格线最大值
                                   values.radar = c(0, 0.4, 0.8), #轴标签显示
                                   background.circle.colour = "white", #背景颜色
                                   group.line.width = 0.5, #线宽
                                   group.point.size = 1, #数据点大小
                                   fill = T, #填充色
                                   fill.alpha = 0.3, #填充色不透明度
                                   group.colours = "#56AEDE") +
  ggtitle("Sensitivity") +#添加标题
  theme(plot.title = element_text(hjust = 0.5, size = 40), #hjust=0.5标题居中，设置标签文字大小
        plot.margin = margin(0, 0, 0, 0, "cm"), #调整边距
        plot.tag = element_text(size = 50, face = "bold") #右上角标签字体大小 
  ) +
  coord_fixed(ratio = 1) #设置图形为正方形

#绘制雷达图-Specificity
UH_test_Specificity_xgb <- ggradar(UH_test_radar_data_xgb[3,], 
                                   grid.line.width = 0.4, #网格线宽度
                                   grid.label.size = 11, #网格线标签字体大小
                                   axis.label.size = 9, #轴标签字体大小
                                   grid.min = 0, #网格线最小值
                                   grid.mid = 0.4, #网格线均值
                                   grid.max = 0.8, #网格线最大值
                                   values.radar = c(0, 0.4, 0.8), #轴标签显示
                                   background.circle.colour = "white", #背景颜色
                                   group.line.width = 0.5, #线宽
                                   group.point.size = 1, #数据点大小
                                   fill = T, #填充色
                                   fill.alpha = 0.2, #填充色不透明度
                                   group.colours = "#EE7A5F") +
  ggtitle("Specificity") + #添加标题
  theme(plot.title = element_text(hjust = 0.5, size = 40), #hjust=0.5标题居中，设置标签文字大小
        plot.margin = margin(0, 0, 0, 0, "cm"), #调整边距
        plot.tag = element_text(size = 50, face = "bold") #右上角标签字体大小 
  ) +
  coord_fixed(ratio = 1) #设置图形为正方形

#绘制雷达图-PPV
UH_test_PPV_xgb <- ggradar(UH_test_radar_data_xgb[4,], 
                           grid.line.width = 0.4, #网格线宽度
                           grid.label.size = 11, #网格线标签字体大小
                           axis.label.size = 9, #轴标签字体大小
                           grid.min = 0, #网格线最小值
                           grid.mid = 0.15, #网格线均值
                           grid.max = 0.3, #网格线最大值
                           values.radar = c(0, 0.15, 0.3), #轴标签显示
                           background.circle.colour = "white", #背景颜色
                           group.line.width = 0.5, #线宽
                           group.point.size = 1, #数据点大小
                           fill = T, #填充色
                           fill.alpha = 0.2, #填充色不透明度
                           group.colours = "#FEAF8A") +
  ggtitle("PPV") + #添加标题
  theme(plot.title = element_text(hjust = 0.5, size = 40), #hjust=0.5标题居中，设置标签文字大小
        plot.margin = margin(0, 0, 0, 0, "cm"), #调整边距
        plot.tag = element_text(size = 50, face = "bold") #右上角标签字体大小 
  ) +
  coord_fixed(ratio = 1) #设置图形为正方形

#绘制雷达图-NPV
UH_test_NPV_xgb <- ggradar(UH_test_radar_data_xgb[5,], 
                           grid.line.width = 0.4, #网格线宽度
                           grid.label.size = 11, #网格线标签字体大小
                           axis.label.size = 9, #轴标签字体大小
                           grid.min = 0, #网格线最小值
                           grid.mid = 0.5, #网格线均值
                           grid.max = 1, #网格线最大值
                           values.radar = c(0, 0.5, 1), #轴标签显示
                           background.circle.colour = "white", #背景颜色
                           group.line.width = 0.5, #线宽
                           group.point.size = 1, #数据点大小
                           fill = T, #填充色
                           fill.alpha = 0.3, #填充色不透明度
                           group.colours = "#F6B7C6") +
  ggtitle("NPV") + #添加标题
  theme(plot.title = element_text(hjust = 0.5, size = 40), #hjust=0.5标题居中，设置标签文字大小
        plot.margin = margin(0, 0, 0, 0, "cm"), #调整边距
        plot.tag = element_text(size = 50, face = "bold") #右上角标签字体大小 
  ) +
  coord_fixed(ratio = 1) #设置图形为正方形

#绘制雷达图-CCR
UH_test_CCR_xgb <- ggradar(UH_test_radar_data_xgb[6,], 
                           grid.line.width = 0.4, #网格线宽度
                           grid.label.size = 11, #网格线标签字体大小
                           axis.label.size = 9, #轴标签字体大小
                           grid.min = 0, #网格线最小值
                           grid.mid = 0.4, #网格线均值
                           grid.max = 0.8, #网格线最大值
                           values.radar = c(0, 0.4, 0.8), #轴标签显示
                           background.circle.colour = "white", #背景颜色
                           group.line.width = 0.5, #线宽
                           group.point.size = 1, #数据点大小
                           fill = T, #填充色
                           fill.alpha = 0.3, #填充色不透明度
                           group.colours = "#D8CBF0") +
  ggtitle("CCR") + #添加标题
  theme(plot.title = element_text(hjust = 0.5, size = 40), #hjust=0.5标题居中，设置标签文字大小
        plot.margin = margin(0, 0, 0, 0, "cm"), #调整边距
        plot.tag = element_text(size = 50, face = "bold") #右上角标签字体大小 
  ) +
  coord_fixed(ratio = 1) #设置图形为正方形

#绘制雷达图-F1 score
UH_test_F1_score_xgb <- ggradar(UH_test_radar_data_xgb[7,], 
                                grid.line.width = 0.4, #网格线宽度
                                grid.label.size = 11, #网格线标签字体大小
                                axis.label.size = 9, #轴标签字体大小
                                grid.min = 0, #网格线最小值
                                grid.mid = 0.2, #网格线均值
                                grid.max = 0.4, #网格线最大值
                                values.radar = c(0, 0.2, 0.4), #轴标签显示
                                background.circle.colour = "white", #背景颜色
                                group.line.width = 0.5, #线宽
                                group.point.size = 1, #数据点大小
                                fill = T, #填充色
                                fill.alpha = 0.3, #填充色不透明度
                                group.colours = "#9FCDC9") +
  ggtitle("F1 score") + #添加标题
  theme(plot.title = element_text(hjust = 0.5, size = 40), #hjust=0.5标题居中，设置标签文字大小
        plot.margin = margin(0, 0, 0, 0, "cm"), #调整边距
        plot.tag = element_text(size = 50, face = "bold") #右上角标签字体大小 
  ) +
  coord_fixed(ratio = 1) #设置图形为正方形

#绘制雷达图-Brier score
UH_test_Brier_score_xgb <- ggradar(UH_test_radar_data_xgb[8,], 
                                   grid.line.width = 0.4, #网格线宽度
                                   grid.label.size = 11, #网格线标签字体大小
                                   axis.label.size = 9, #轴标签字体大小
                                   grid.min = 0, #网格线最小值
                                   grid.mid = 0.05, #网格线均值
                                   grid.max = 0.1, #网格线最大值
                                   values.radar = c(0, 0.05, 0.1), #轴标签显示
                                   background.circle.colour = "white", #背景颜色
                                   group.line.width = 0.5, #线宽
                                   group.point.size = 1, #数据点大小
                                   fill = T, #填充色
                                   fill.alpha = 0.2, #填充色不透明度
                                   group.colours = "#7EB87B") +
  ggtitle("Brier score") + #添加标题
  theme(plot.title = element_text(hjust = 0.5, size = 40), #hjust=0.5标题居中，设置标签文字大小
        plot.margin = margin(0, 0, 0, 0, "cm"), #调整边距
        plot.tag = element_text(size = 50, face = "bold") #右上角标签字体大小 
  ) +
  coord_fixed(ratio = 1) #设置图形为正方形

#合并测试队列各性能雷达图
UH_test_radar_xgb <- (UH_test_AUC_xgb + UH_test_Sensitivity_xgb + UH_test_Specificity_xgb + UH_test_PPV_xgb + 
                        UH_test_NPV_xgb + UH_test_CCR_xgb + UH_test_F1_score_xgb + UH_test_Brier_score_xgb) + 
  plot_annotation(tag_levels = "A") + #自动添加A和B标签
  plot_layout(ncol = 4, nrow = 2) #图片2*4

#导出测试队列各性能雷达图
ggsave(plot = UH_test_radar_xgb, 
       filename = "UH_test_radar_xgb.png", 
       width = 17, 
       height = 8.5,
       units = "cm", 
       dpi = 600)


#合并两组的模型性能结果
LH_UH_test_metrics_xgb <- rbind(LH_test_metrics_xgb, UH_test_metrics_xgb)

#合并两组的delong test结果
test_roc_comparisons_xgb <- merge(LH_test_auc_comparisons_xgb, UH_test_auc_comparisons_xgb, 
                                  by = "Model_comparison")

#合并两组ROC图、校准曲线图、DCA图
ROC_calibration_dca_xgb <- (LH_test_roc_xgb + UH_test_roc_xgb) / 
  (LH_test_calibration_xgb + UH_test_calibration_xgb) / 
  (LH_test_dca_xgb + UH_test_dca_xgb) + 
  plot_annotation(tag_levels = "A") #自动添加A和B标签

#导出两组ROC图、校准曲线图、DCA图
ggsave(plot = ROC_calibration_dca_xgb, 
       filename = "ROC_calibration_dca_xgb.png", 
       width = 16, 
       height = 20,
       units = "cm", 
       dpi = 600)

##合并两组各性能雷达图
#只给两组图加标签
LH_test_AUC_xgb_tag <- LH_test_AUC_xgb +
  labs(tag = "A") +
  theme(plot.tag.position = c(0.02, 1),
        plot.tag = element_text(size = 60, face = "bold"))

UH_test_AUC_xgb_tag <- UH_test_AUC_xgb +
  labs(tag = "B") +
  theme(plot.tag.position = c(0.02, 1),
        plot.tag = element_text(size = 60, face = "bold"))

LH_UH_test_radar_xgb <- (
  LH_test_AUC_xgb_tag + LH_test_Sensitivity_xgb + LH_test_Specificity_xgb + LH_test_PPV_xgb +
    LH_test_NPV_xgb + LH_test_CCR_xgb + LH_test_F1_score_xgb + LH_test_Brier_score_xgb +
    UH_test_AUC_xgb_tag + UH_test_Sensitivity_xgb + UH_test_Specificity_xgb + UH_test_PPV_xgb +
    UH_test_NPV_xgb + UH_test_CCR_xgb + UH_test_F1_score_xgb + UH_test_Brier_score_xgb
) +
  plot_layout(ncol = 4, nrow = 4)

#导出测试队列各性能雷达图
ggsave(plot = LH_UH_test_radar_xgb, 
       filename = "LH_UH_test_radar_xgb.png", 
       width = 17, 
       height = 15,
       units = "cm", 
       dpi = 600)


###------------------SHAP解释------------------
##-----SHAP解释：Low decline和High-stable组-----
#构建SHAP可视化对象
LH_sv_xgb <- shapviz(LH_round_outputs[[7]][["shap_value"]], which_class = 2) 
#数据字典映射变量名
LH_shap_values_df_xgb <- as.data.frame(LH_sv_xgb$S)
colnames(LH_shap_values_df_xgb) <- variable_dict$display_name[match(names(LH_shap_values_df_xgb), variable_dict$analysis_name)]
LH_sv_xgb$S <- as.matrix(LH_shap_values_df_xgb)
colnames(LH_sv_xgb$X) <- variable_dict$display_name[match(names(LH_sv_xgb$X), variable_dict$analysis_name)]

#SHAP条形图
LH_shap_bar <- sv_importance(LH_sv_xgb, 
                             kind = "bar", #条形图
                             max_display = Inf, #显示所有特征
                             show_numbers = T, #显示数值（即特征的重要性得分） 
                             fill = "#48EDFE") + #条形图填充颜色
  scale_x_continuous(limits = c(0, 0.06), #限制x轴刻度
                     breaks = seq(0, 0.06, by = 0.02)) + #设置x轴刻度
  theme(
    axis.text.y = element_text(vjust = 0.5, hjust = 1, margin = margin(r = 1)),
    panel.background = element_rect(fill = "white"), #面板背景为白色
    plot.background = element_rect(fill = "white"), #整个绘图区域背景为白色
    panel.grid.major.y = element_blank(), #去掉横向网格线
    panel.grid.minor = element_blank(), #去掉次要网格线
    axis.title = element_text(size = 40), #坐标轴标题字体大小
    axis.text = element_text(size = 30), #坐标轴刻度字体大小
    plot.tag = element_text(size = 70, face = "bold"), #设置标签文字大小
    plot.margin = margin(0, 0, 0, 0, "cm"), #调整边距
    axis.ticks = element_line(size = 0.25), #y轴刻度线粗细
    axis.ticks.length = unit(0.05, "cm"), #刻度线在外侧及长度
    axis.ticks.y = element_blank() #去掉y轴刻度线
  )

#SHAP条形图上面数字字体大小
LH_shap_bar$layers <- lapply(LH_shap_bar$layers, function(layer) {
  if ("GeomText" %in% class(layer$geom)) {
    layer$aes_params <- modifyList(layer$aes_params, list(size = 10))
  }
  layer
})

#SHAP摘要图
LH_shap_beeswarm <- sv_importance(LH_sv_xgb, 
                                  kind = "beeswarm", #蜂群图
                                  size = 0.2, # 散点大小（蜂群图）
                                  bee_width = 0.3, #散点的水平扩展宽度
                                  max_display = Inf) + #显示所有特征
  scale_color_gradient(low = "#6601F7", high = "#48EDFE") + 
  scale_x_continuous(limits = c(-0.1, 0.6), #限制x轴刻度
                     breaks = seq(-0.1, 0.6, by = 0.1)) + #设置x轴刻度
  theme(
    panel.background = element_rect(fill = "white"), #面板背景为白色
    plot.background = element_rect(fill = "white"), #整个绘图区域背景为白色
    axis.title = element_text(size = 40), #坐标轴标题字体大小
    axis.text = element_text(size = 30), #坐标轴刻度字体大小
    legend.title = element_text(size = 30), #图例标题字体大小
    legend.text = element_text(size = 20), #图例字体大小
    legend.key.height = unit(0.35, "cm"), #条形图例高度
    legend.key.width = unit(0.1, "cm"), #条形图例宽度
    plot.tag = element_text(size = 70, face = "bold"), #设置标签文字大小
    plot.margin = margin(0, 0, 0, 0, "cm"), #调整边距
    axis.ticks = element_line(size = 0.25), #y轴刻度线粗细
    axis.ticks.length = unit(0.05, "cm"), #刻度线在外侧及长度
    axis.ticks.y = element_blank() #去掉y轴刻度线
  )

#合并SHAP条形图、摘要图
LH_shap_bar_beeswarm <- LH_shap_bar + LH_shap_beeswarm +
  plot_annotation(tag_levels = "A") #自动添加A和B标签

#导出SHAP条形图、摘要图
ggsave(plot = LH_shap_bar_beeswarm, 
       filename = "LH_shap_bar_beeswarm.png", 
       width = 17, 
       height = 3.5,
       units = "cm",
       bg = "white",
       dpi = 600)

#对第一个观测画出SHAP瀑布图
LH_shap_waterfall <- sv_waterfall(LH_sv_xgb, row_id = 1, size = 300, 
                                  fill_colors = c("#48EDFE", "#6601F7"), #颜色
                                  annotation_size = 10) + #注释文本字体大小
  labs(x = "SHAP value", tag = "C") + #修改x轴标题为"SHAP value"，添加标签C
  scale_x_continuous(limits = c(0.02, 0.12), #限制x轴刻度
                     breaks = seq(0.02, 0.12, by = 0.02)) + #设置x轴刻度
  theme(
    axis.title = element_text(size = 40), #坐标轴标题字体大小
    axis.text = element_text(size = 30), #坐标轴刻度字体大小
    axis.ticks = element_line(size = 0.25), #y轴刻度线粗细
    axis.ticks.length = unit(0.05, "cm"), #刻度线在外侧及长度
    axis.line.x = element_line(size = 0.25),  # 设置x轴主轴线粗细
    panel.grid = element_blank(), #去掉网格线
    plot.margin = margin(0, 0, 0, 0, "cm"), #调整边距
    plot.tag = element_text(size = 70, face = "bold") #设置标签文字大小
  ) +
  coord_fixed(ratio = 8/1035) #设置图形为长方形

#SHAP力图（单个样本）
LH_shap_force <- sv_force(LH_sv_xgb, row_id = 1, max_display = Inf, size = 30, 
                          fill_colors = c("#48EDFE", "#6601F7"), #颜色
                          bar_label_size = 10, #条形标签字体大小
                          annotation_size = 10) + #注释文本字体大小
  labs(x = "SHAP value", tag = "D") + #修改x轴标题为"SHAP value"，添加标签D
  theme(
    axis.title = element_text(size = 40), #坐标轴标题字体大小
    axis.text = element_text(size = 30), #坐标轴刻度字体大小
    axis.ticks = element_line(size = 0.25), #y轴刻度线粗细
    axis.ticks.length = unit(0.05, "cm"), #刻度线在外侧及长度
    axis.line.x = element_line(size = 0.25),  # 设置x轴主轴线粗细
    panel.grid = element_blank(), #去掉网格线
    plot.margin = margin(0, 0, 0, 0, "cm"), #调整边距
    plot.tag = element_text(size = 70, face = "bold") #设置标签文字大小
  )

#合并SHAP瀑布图、力图
LH_shap_waterfall_force <- LH_shap_waterfall + LH_shap_force

#导出SHAP瀑布图、力图
ggsave(plot = LH_shap_waterfall_force, 
       filename = "LH_shap_waterfall_force.png", 
       width = 17, 
       height = 3.5,
       units = "cm",
       bg = "white",
       dpi = 600)


##SHAP变量依赖图
#定义变量
LH_x <- c("Age", "Waist circumference", "FBG", "UA", "WBC")
#绘图
LH_dependence_plots <- lapply(LH_x, function(v) {
  sv_dependence(LH_sv_xgb, v = v, color_var = "auto", size = 0.03) +
    scale_color_gradient(low = "#6601F7", high = "#48EDFE") + #颜色
    theme_bw() + 
    theme(
      axis.title = element_text(size = 40), #坐标轴标题字体大小
      axis.text = element_text(size = 30), #坐标轴刻度字体大小
      legend.title = element_text(size = 25), #图例标题字体大小
      legend.text = element_text(size = 20), #图例字体大小
      legend.key.height = unit(0.6, "cm"), #条形图例高度
      legend.key.width = unit(0.1, "cm"), #条形图例宽度
      plot.margin = margin(0, 0, 0, 0, "cm"), #调整边距
      axis.ticks = element_line(size = 0.2), #y轴刻度线粗细
      axis.ticks.length = unit(0.03, "cm"), #刻度线在外侧及长度
      aspect.ratio = 1, #图形为正方形
      legend.position = "right", #图例在右侧
      plot.tag = element_text(size = 70, face = "bold") #设置标签文字大小
    )
})

#拼图
LH_shap_dependence_plot <- wrap_plots(
  lapply(LH_dependence_plots, \(p) p + theme(plot.margin = margin(0, 5, 10, 0))), # 上右下左
  ncol = 3, nrow = 2) + 
  plot_annotation(tag_levels = list("E"))

#导出SHAP变量依赖图
ggsave(plot = LH_shap_dependence_plot, 
       filename = "LH_shap_dependence_plot.png", 
       width = 17, 
       height = 9.5,
       units = "cm",
       bg = "white",
       dpi = 600)


##-----SHAP解释：Unstable和High-stable组-----
#构建SHAP可视化对象
UH_sv_xgb <- shapviz(UH_round_outputs[[7]][["shap_value"]], which_class = 2) 
#数据字典映射变量名
UH_shap_values_df_xgb <- as.data.frame(UH_sv_xgb$S)
colnames(UH_shap_values_df_xgb) <- variable_dict$display_name[match(names(UH_shap_values_df_xgb), variable_dict$analysis_name)]
UH_sv_xgb$S <- as.matrix(UH_shap_values_df_xgb)
colnames(UH_sv_xgb$X) <- variable_dict$display_name[match(names(UH_sv_xgb$X), variable_dict$analysis_name)]

#SHAP条形图
UH_shap_bar <- sv_importance(UH_sv_xgb, 
                             kind = "bar", #条形图
                             max_display = Inf, #显示所有特征
                             show_numbers = T, #显示数值（即特征的重要性得分） 
                             fill = "#48EDFE") + #条形图填充颜色
  scale_x_continuous(limits = c(0, 0.06), #限制x轴刻度
                     breaks = seq(0, 0.06, by = 0.02)) + #设置x轴刻度
  theme(
    axis.text.y = element_text(vjust = 0.5, hjust = 1, margin = margin(r = 1)),
    panel.background = element_rect(fill = "white"), #面板背景为白色
    plot.background = element_rect(fill = "white"), #整个绘图区域背景为白色
    panel.grid.major.y = element_blank(), #去掉横向网格线
    panel.grid.minor = element_blank(), #去掉次要网格线
    axis.title = element_text(size = 40), #坐标轴标题字体大小
    axis.text = element_text(size = 30), #坐标轴刻度字体大小
    plot.tag = element_text(size = 70, face = "bold"), #设置标签文字大小
    plot.margin = margin(0, 0, 0, 0, "cm"), #调整边距
    axis.ticks = element_line(size = 0.25), #y轴刻度线粗细
    axis.ticks.length = unit(0.05, "cm"), #刻度线在外侧及长度
    axis.ticks.y = element_blank() #去掉y轴刻度线
  )

#SHAP条形图上面数字字体大小
UH_shap_bar$layers <- lapply(UH_shap_bar$layers, function(layer) {
  if ("GeomText" %in% class(layer$geom)) {
    layer$aes_params <- modifyList(layer$aes_params, list(size = 10))
  }
  layer
})

#SHAP摘要图
UH_shap_beeswarm <- sv_importance(UH_sv_xgb, 
                                  kind = "beeswarm", #蜂群图
                                  size = 0.2, # 散点大小（蜂群图）
                                  bee_width = 0.3, #散点的水平扩展宽度
                                  max_display = Inf) + #显示所有特征
  scale_color_gradient(low = "#6601F7", high = "#48EDFE") + 
  scale_x_continuous(limits = c(-0.2, 0.8), #限制x轴刻度
                     breaks = seq(-0.2, 0.8, by = 0.2)) + #设置x轴刻度
  theme(
    panel.background = element_rect(fill = "white"), #面板背景为白色
    plot.background = element_rect(fill = "white"), #整个绘图区域背景为白色
    axis.title = element_text(size = 40), #坐标轴标题字体大小
    axis.text = element_text(size = 30), #坐标轴刻度字体大小
    legend.title = element_text(size = 30), #图例标题字体大小
    legend.text = element_text(size = 20), #图例字体大小
    legend.key.height = unit(0.35, "cm"), #条形图例高度
    legend.key.width = unit(0.1, "cm"), #条形图例宽度
    plot.tag = element_text(size = 70, face = "bold"), #设置标签文字大小
    plot.margin = margin(0, 0, 0, 0, "cm"), #调整边距
    axis.ticks = element_line(size = 0.25), #y轴刻度线粗细
    axis.ticks.length = unit(0.05, "cm"), #刻度线在外侧及长度
    axis.ticks.y = element_blank() #去掉y轴刻度线
  )

#合并SHAP条形图、摘要图
UH_shap_bar_beeswarm <- UH_shap_bar + UH_shap_beeswarm +
  plot_annotation(tag_levels = "A") #自动添加A和B标签

#导出SHAP条形图、摘要图
ggsave(plot = UH_shap_bar_beeswarm, 
       filename = "UH_shap_bar_beeswarm.png", 
       width = 17, 
       height = 3.5,
       units = "cm",
       bg = "white",
       dpi = 600)

#对第一个观测画出SHAP瀑布图
UH_shap_waterfall <- sv_waterfall(UH_sv_xgb, row_id = 1, size = 300, 
                                  fill_colors = c("#48EDFE", "#6601F7"), #颜色
                                  annotation_size = 10) + #注释文本字体大小
  labs(x = "SHAP value", tag = "C") + #修改x轴标题为"SHAP value"，添加标签C
  scale_x_continuous(limits = c(0.06, 0.13), #限制x轴刻度
                     breaks = seq(0.06, 0.13, by = 0.01)) + #设置x轴刻度
  theme(
    axis.title = element_text(size = 40), #坐标轴标题字体大小
    axis.text = element_text(size = 30), #坐标轴刻度字体大小
    axis.ticks = element_line(size = 0.25), #y轴刻度线粗细
    axis.ticks.length = unit(0.05, "cm"), #刻度线在外侧及长度
    axis.line.x = element_line(size = 0.25),  # 设置x轴主轴线粗细
    panel.grid = element_blank(), #去掉网格线
    plot.margin = margin(0, 0, 0, 0, "cm"), #调整边距
    plot.tag = element_text(size = 70, face = "bold") #设置标签文字大小
  ) +
  coord_fixed(ratio = 8/1780) #设置图形为长方形

#SHAP力图（单个样本）
UH_shap_force <- sv_force(UH_sv_xgb, row_id = 1, max_display = Inf, size = 30, 
                          fill_colors = c("#48EDFE", "#6601F7"), #颜色
                          bar_label_size = 10, #条形标签字体大小
                          annotation_size = 10) + #注释文本字体大小
  labs(x = "SHAP value", tag = "D") + #修改x轴标题为"SHAP value"，添加标签D
  theme(
    axis.title = element_text(size = 40), #坐标轴标题字体大小
    axis.text = element_text(size = 30), #坐标轴刻度字体大小
    axis.ticks = element_line(size = 0.25), #y轴刻度线粗细
    axis.ticks.length = unit(0.05, "cm"), #刻度线在外侧及长度
    axis.line.x = element_line(size = 0.25),  # 设置x轴主轴线粗细
    panel.grid = element_blank(), #去掉网格线
    plot.margin = margin(0, 0, 0, 0, "cm"), #调整边距
    plot.tag = element_text(size = 70, face = "bold") #设置标签文字大小
  )

#合并SHAP瀑布图、力图
UH_shap_waterfall_force <- UH_shap_waterfall + UH_shap_force

#导出SHAP瀑布图、力图
ggsave(plot = UH_shap_waterfall_force, 
       filename = "UH_shap_waterfall_force.png", 
       width = 17, 
       height = 3.5,
       units = "cm",
       bg = "white",
       dpi = 600)


##SHAP变量依赖图
#定义变量
UH_x <- c("Age", "HDL-C", "LDL-C", "FBG", "WBC")
#绘图
UH_dependence_plots <- lapply(UH_x, function(v) {
  sv_dependence(UH_sv_xgb, v = v, color_var = "auto", size = 0.03) +
    scale_color_gradient(low = "#6601F7", high = "#48EDFE") + #颜色
    theme_bw() + 
    theme(
      axis.title = element_text(size = 40), #坐标轴标题字体大小
      axis.text = element_text(size = 30), #坐标轴刻度字体大小
      legend.title = element_text(size = 25), #图例标题字体大小
      legend.text = element_text(size = 20), #图例字体大小
      legend.key.height = unit(0.56, "cm"), #条形图例高度
      legend.key.width = unit(0.1, "cm"), #条形图例宽度
      plot.margin = margin(0, 0, 0, 0, "cm"), #调整边距
      axis.ticks = element_line(size = 0.2), #y轴刻度线粗细
      axis.ticks.length = unit(0.03, "cm"), #刻度线在外侧及长度
      aspect.ratio = 1, #图形为正方形
      legend.position = "right", #图例在右侧
      plot.tag = element_text(size = 70, face = "bold") #设置标签文字大小
    )
})

#拼图
UH_shap_dependence_plot <- wrap_plots(
  lapply(UH_dependence_plots, \(p) p + theme(plot.margin = margin(0, 5, 5, 0))), # 上右下左
  ncol = 3, nrow = 2) + 
  plot_annotation(tag_levels = list("E"))

#导出SHAP变量依赖图
ggsave(plot = UH_shap_dependence_plot, 
       filename = "UH_shap_dependence_plot.png", 
       width = 17, 
       height = 9,
       units = "cm",
       bg = "white",
       dpi = 600)


###------------------桑基图------------------
##-----桑基图：Low decline和High-stable组-----
#创建数据集
LH_test_sankey <- data_LH_test %>%
  select(Age, Waist_circumference, FBG, UA, WBC, CMDs) %>%
  mutate(
    # Age分类：>=60 years和45-59 years
    Age_group = ifelse(Age >= 60, "≥60 years", "45-59 years"),
    
    # Waist_circumference按四分位数分组
    Waist_circumference_group = cut(Waist_circumference,
                                    breaks = quantile(Waist_circumference, 
                                                      probs = c(0, 0.25, 0.5, 0.75, 1),
                                                      na.rm = TRUE),
                                    labels = c("Q1", "Q2", "Q3", "Q4"),
                                    include.lowest = TRUE),
    
    # FBG按四分位数分组
    FBG_group = cut(FBG,
                    breaks = quantile(FBG, 
                                      probs = c(0, 0.25, 0.5, 0.75, 1),
                                      na.rm = TRUE),
                    labels = c("Q1", "Q2", "Q3", "Q4"),
                    include.lowest = TRUE),
    
    # UA按四分位数分组
    UA_group = cut(UA,
                   breaks = quantile(UA, 
                                     probs = c(0, 0.25, 0.5, 0.75, 1),
                                     na.rm = TRUE),
                   labels = c("Q1", "Q2", "Q3", "Q4"),
                   include.lowest = TRUE),
    
    # WBC按四分位数分组
    WBC_group = cut(WBC,
                    breaks = quantile(WBC, 
                                      probs = c(0, 0.25, 0.5, 0.75, 1),
                                      na.rm = TRUE),
                    labels = c("Q1", "Q2", "Q3", "Q4"),
                    include.lowest = TRUE)
  ) %>%
  # 选择需要的列，并重新命名以保持一致性
  select(Age_group, 
         Waist_circumference_group,
         FBG_group,
         UA_group,
         WBC_group,
         CMDs) %>%
  # 重命名列名，去掉_group后缀（可选）
  rename(
    Age = Age_group,
    "Waist circumference" = Waist_circumference_group,
    FBG = FBG_group,
    UA = UA_group,
    WBC = WBC_group
  )

#转换为长格式数据框
data_LH_sankey <- LH_test_sankey %>%
  make_long(Age, "Waist circumference", FBG, UA, WBC, CMDs)

#定义节点颜色
cols <- c("#90EDFE", "#D8CBF0", "#A0E7E5","#B5EAD7", "#F6B7C6","#FFDAC1", 
          "#C1CEEA", "#EE7A5F")
#绘图
LH_sankey_plot <- ggplot(data_LH_sankey, aes(x = x,
                                             next_x = next_x,
                                             node = node,
                                             next_node = next_node,
                                             fill = factor(node),
                                             label = node)) + #添加 label 映射，用于显示文字
  geom_sankey(flow.alpha = 0.6, #设置连线透明度
              node.color = "black", #给节点添加黑色边框
              node.linewidth = 0.1, #设置边框粗细
              width = 0.2, #调整节点宽度
              space = 0) + 
  scale_fill_manual(values = cols) +
  labs(tag = "A") + #添加标签A
  theme_sankey(base_size = 16) + #使用 ggsankey 自带的简洁主题
  theme(axis.text = element_text(size = 35), #坐标轴刻度字体大小
        legend.text = element_text(size = 25, #图例字体大小
                                   margin = margin(l = 0.1, unit = "cm")), #图例与字体间距
        legend.key.height = unit(0.2, "cm"), #条形图例高度
        legend.key.width = unit(0.2, "cm"), #条形图例宽度
        plot.margin = margin(0, 0.5, 0, 0, "cm"), #调整边距
        legend.position = c(1, 0.5), #图例在右侧
        legend.margin = margin(0, 0, 0, 0, "cm"), #图例边距
        plot.tag.position = c(0.03, 0.95), #图标签位置
        plot.tag = element_text(size = 70, face = "bold") #设置标签文字大小
  ) +
  guides(fill = guide_legend(title = NULL)) + #隐藏图例标题
  labs(x = NULL, title = "") + #移除多余的轴标签
  coord_fixed(ratio = 5/3300) #设置图形为长方形

##-----桑基图：Unstable和High-stable组-----
#创建数据集
UH_test_sankey <- data_UH_test %>%
  select(Age, HDL_C, LDL_C, FBG, WBC, CMDs) %>%
  mutate(
    # Age分类：>=60 years和45-59 years
    Age_group = ifelse(Age >= 60, "≥60 years", "45-59 years"),
    
    # HDL_C按四分位数分组
    HDL_C_group = cut(HDL_C,
                      breaks = quantile(HDL_C, 
                                        probs = c(0, 0.25, 0.5, 0.75, 1),
                                        na.rm = TRUE),
                      labels = c("Q1", "Q2", "Q3", "Q4"),
                      include.lowest = TRUE),
    
    # LDL_C按四分位数分组
    LDL_C_group = cut(LDL_C,
                      breaks = quantile(LDL_C, 
                                        probs = c(0, 0.25, 0.5, 0.75, 1),
                                        na.rm = TRUE),
                      labels = c("Q1", "Q2", "Q3", "Q4"),
                      include.lowest = TRUE),
    
    # FBG按四分位数分组
    FBG_group = cut(FBG,
                    breaks = quantile(FBG, 
                                      probs = c(0, 0.25, 0.5, 0.75, 1),
                                      na.rm = TRUE),
                    labels = c("Q1", "Q2", "Q3", "Q4"),
                    include.lowest = TRUE),
    
    # WBC按四分位数分组
    WBC_group = cut(WBC,
                    breaks = quantile(WBC, 
                                      probs = c(0, 0.25, 0.5, 0.75, 1),
                                      na.rm = TRUE),
                    labels = c("Q1", "Q2", "Q3", "Q4"),
                    include.lowest = TRUE)
  ) %>%
  # 选择需要的列，并重新命名以保持一致性
  select(Age_group, 
         HDL_C_group,
         LDL_C_group,
         FBG_group,
         WBC_group,
         CMDs) %>%
  # 重命名列名，去掉_group后缀（可选）
  rename(
    Age = Age_group,
    "HDL-C" = HDL_C_group,
    "LDL-C" = LDL_C_group,
    FBG = FBG_group,
    WBC = WBC_group
  )

#转换为长格式数据框
data_UH_sankey <- UH_test_sankey %>%
  make_long(Age, "HDL-C", "LDL-C", FBG, WBC, CMDs)

#绘图
UH_sankey_plot <- ggplot(data_UH_sankey, aes(x = x,
                                             next_x = next_x,
                                             node = node,
                                             next_node = next_node,
                                             fill = factor(node),
                                             label = node)) + #添加 label 映射，用于显示文字
  geom_sankey(flow.alpha = 0.6, #设置连线透明度
              node.color = "black", #给节点添加黑色边框
              node.linewidth = 0.1, #设置边框粗细
              width = 0.2, #调整节点宽度
              space = 0) + 
  scale_fill_manual(values = cols) +
  labs(tag = "B") + #添加标签B
  theme_sankey(base_size = 16) + #使用 ggsankey 自带的简洁主题
  theme(axis.text = element_text(size = 35), #坐标轴刻度字体大小
        legend.text = element_text(size = 25, #图例字体大小
                                   margin = margin(l = 0.1, unit = "cm")), #图例与字体间距
        legend.key.height = unit(0.2, "cm"), #条形图例高度
        legend.key.width = unit(0.2, "cm"), #条形图例宽度
        plot.margin = margin(0, 0.5, 0, 0, "cm"), #调整边距
        legend.position = c(1, 0.5), #图例在右侧
        legend.margin = margin(0, 0, 0, 0, "cm"), #图例边距
        plot.tag.position = c(0.03, 0.95), #图标签位置
        plot.tag = element_text(size = 70, face = "bold") #设置标签文字大小
  ) +
  guides(fill = guide_legend(title = NULL)) + #隐藏图例标题
  labs(x = NULL, title = "") + #移除多余的轴标签
  coord_fixed(ratio = 5/4600) #设置图形为长方形

#合并两组桑基图
LH_UH_sankey_plot <- LH_sankey_plot + UH_sankey_plot

#导出两组桑基图
ggsave(plot = LH_UH_sankey_plot, 
       filename = "LH_UH_sankey_plot.png", 
       width = 17, 
       height = 5.5,
       units = "cm",
       bg = "white",
       dpi = 600)


###-----------敏感性分析-----------
##---------(1)敏感性分析1：完整数据---------
#删除有缺失的行
sensitivity_data1 <- na.omit(data_analyze2)

###------划分数据集------
##轨迹组划分
#Low decline和High-stable组
sensitivity_data1_LH <- sensitivity_data1 %>%
  filter(eGFR_group %in% c(1, 3)) %>%
  select(-eGFR_group)  # 删除 eGFR_group 列

#Unstable和High-stable组
sensitivity_data1_UH <- sensitivity_data1 %>%
  filter(eGFR_group %in% c(2, 3)) %>%
  select(-eGFR_group)  # 删除 eGFR_group 列

##Low decline和High-stable组划分训练队列与测试队列
#设置随机种子，确保划分可重复性
set.seed(1217)
#createDataPartition()会自动从y的各个level随机取出等比例的数据来，p表示训练队列与测试队列占比，list = F表示返回的结果是一个向量，times = 1表示分割操作只执行一次 
sensitivity_data1_trainindex_LH <- createDataPartition(sensitivity_data1_LH$CMDs, p = 0.7, list = F, times = 1)
#划分出训练队列
sensitivity_data1_LH_train <- sensitivity_data1_LH[sensitivity_data1_trainindex_LH, ]
#划分出测试队列
sensitivity_data1_LH_test <- sensitivity_data1_LH[-sensitivity_data1_trainindex_LH, ]

##Unstable和High-stable组划分训练队列与测试队列
#设置随机种子，确保划分可重复性
set.seed(1217)
#createDataPartition()会自动从y的各个level随机取出等比例的数据来，p表示训练队列与测试队列占比，list = F表示返回的结果是一个向量，times = 1表示分割操作只执行一次 
sensitivity_data1_trainindex_UH <- createDataPartition(sensitivity_data1_UH$CMDs, p = 0.7, list = F, times = 1)
#划分出训练队列
sensitivity_data1_UH_train <- sensitivity_data1_UH[sensitivity_data1_trainindex_UH, ]
#划分出测试队列
sensitivity_data1_UH_test <- sensitivity_data1_UH[-sensitivity_data1_trainindex_UH, ]

###-----------变量标准化-----------
##Low decline和High-stable组变量标准化
#使用preProcess()对训练队列进行标准化处理（中心化和缩放），使得每列数据的均值为0，标准差为1
sensitivity_data1_std_para_LH <- preProcess(sensitivity_data1_LH_train, method = c("center", "scale"))
#对训练队列进行标准化
sensitivity_data1_LH_train_std <- predict(sensitivity_data1_std_para_LH, newdata = sensitivity_data1_LH_train)
#对测试队列进行标准化，使用训练队列的标准化参数
sensitivity_data1_LH_test_std <- predict(sensitivity_data1_std_para_LH, newdata = sensitivity_data1_LH_test)

##Unstable和High-stable组变量标准化
#使用preProcess()对训练队列进行标准化处理（中心化和缩放），使得每列数据的均值为0，标准差为1
sensitivity_data1_std_para_UH <- preProcess(sensitivity_data1_UH_train, method = c("center", "scale"))
#对训练队列进行标准化
sensitivity_data1_UH_train_std <- predict(sensitivity_data1_std_para_UH, newdata = sensitivity_data1_UH_train)
#对测试队列进行标准化，使用训练队列的标准化参数
sensitivity_data1_UH_test_std <- predict(sensitivity_data1_std_para_UH, newdata = sensitivity_data1_UH_test)

##XGBoost、SVM不能处理因子变量，数据集转换为数值型，除了CMDs
sensitivity_data1_LH_train_std_num <- to_num_except_CMDs(sensitivity_data1_LH_train_std)
sensitivity_data1_LH_test_std_num <- to_num_except_CMDs(sensitivity_data1_LH_test_std)
sensitivity_data1_UH_train_std_num <- to_num_except_CMDs(sensitivity_data1_UH_train_std)
sensitivity_data1_UH_test_std_num <- to_num_except_CMDs(sensitivity_data1_UH_test_std)

#提取选中的特征数据和结局数据
sensitivity_data1_LH5_train <- sensitivity_data1_LH_train_std_num[, c(LH_round_outputs[[7]][["features"]], "CMDs")]
sensitivity_data1_LH5_test <- sensitivity_data1_LH_test_std_num[, c(LH_round_outputs[[7]][["features"]], "CMDs")]
sensitivity_data1_UH5_train <- sensitivity_data1_UH_train_std_num[, c(UH_round_outputs[[7]][["features"]], "CMDs")]
sensitivity_data1_UH5_test <- sensitivity_data1_UH_test_std_num[, c(UH_round_outputs[[7]][["features"]], "CMDs")]

##------构建预测模型------
#创建任务
sensitivity1_LH_train_task <- as_task_classif(sensitivity_data1_LH5_train, target = "CMDs")
sensitivity1_LH_test_task <- as_task_classif(sensitivity_data1_LH5_test, target = "CMDs")
sensitivity1_UH_train_task <- as_task_classif(sensitivity_data1_UH5_train, target = "CMDs")
sensitivity1_UH_test_task <- as_task_classif(sensitivity_data1_UH5_test, target = "CMDs")

##训练、预测
#Low decline和High-stable组：XGBoost
sensitivity1_LH_xgb_learner <- lrn("classif.xgboost", predict_type = "prob", verbose = 0)
sensitivity1_LH_xgb_learner$param_set$values <- list(
  nrounds = to_tune(500, 1000), #树的数量：控制迭代次数，决定模型学习的程度。
  max_depth = to_tune(1, 10), #树的最大深度：控制树的复杂度，防止过拟合。
  eta = to_tune(0.001, 0.05), #学习率：控制学习的速度，影响收敛和稳定性。
  min_child_weight = to_tune(1, 10), #每个叶节点最小实例权重：限制分裂，防止过拟合。
  subsample = to_tune(0.1, 0.5) #子样本比例：提供随机性，减少过拟合风险，增加多样性。
)
set.seed(123)
sensitivity1_LH_xgb <- tune(tuner = tnr("grid_search", resolution = 5),
                            task = sensitivity1_LH_train_task,
                            learner = sensitivity1_LH_xgb_learner,
                            resampling = rsmp("cv", folds = 5),
                            measure = msr("classif.auc")
)
sensitivity1_LH_xgb_learner$param_set$values <- sensitivity1_LH_xgb$result_learner_param_vals
sensitivity1_LH_xgb_learner$train(sensitivity1_LH_train_task)
sensitivity1_LH_train_xgb_pred <- sensitivity1_LH_xgb_learner$predict(sensitivity1_LH_train_task)
sensitivity1_LH_test_xgb_pred <- sensitivity1_LH_xgb_learner$predict(sensitivity1_LH_test_task)

#Unstable和High-stable组：XGBoost
sensitivity1_UH_xgb_learner <- lrn("classif.xgboost", predict_type = "prob", verbose = 0)
sensitivity1_UH_xgb_learner$param_set$values <- list(
  nrounds = to_tune(100, 500), #树的数量：控制迭代次数，决定模型学习的程度。
  max_depth = to_tune(1, 10), #树的最大深度：控制树的复杂度，防止过拟合。
  eta = to_tune(0.001, 0.05), #学习率：控制学习的速度，影响收敛和稳定性。
  min_child_weight = to_tune(1, 10), #每个叶节点最小实例权重：限制分裂，防止过拟合。
  subsample = to_tune(0.5, 1) #子样本比例：提供随机性，减少过拟合风险，增加多样性。
)
set.seed(123)
sensitivity1_UH_xgb <- tune(tuner = tnr("grid_search", resolution = 5),
                            task = sensitivity1_UH_train_task,
                            learner = sensitivity1_UH_xgb_learner,
                            resampling = rsmp("cv", folds = 5),
                            measure = msr("classif.auc")
)
sensitivity1_UH_xgb_learner$param_set$values <- sensitivity1_UH_xgb$result_learner_param_vals
sensitivity1_UH_xgb_learner$train(sensitivity1_UH_train_task)
sensitivity1_UH_train_xgb_pred <- sensitivity1_UH_xgb_learner$predict(sensitivity1_UH_train_task)
sensitivity1_UH_test_xgb_pred <- sensitivity1_UH_xgb_learner$predict(sensitivity1_UH_test_task)
save(sensitivity1_UH_train_xgb_pred,
     sensitivity1_UH_test_xgb_pred,
     file = "C:/Users/74077/Desktop/sensitivity1_UH5_xgb_pred_eGFR4.RData")

###------性能评估------
#训练队列模型性能评估函数
train_performance <- function(train_pred, train_data, Dataset_name) {
  #获取训练队列模型的预测概率
  pred_prob_train <- train_pred$prob[, "Yes"]
  
  #获取训练队列的实际分类
  true_class_train <- ifelse(train_data$CMDs == "Yes", 1, 0)
  
  roc_train <- roc(true_class_train, pred_prob_train)
  auc_train <- roc_train$auc
  ci_train <- ci(roc_train)
  #合并AUC和95%CI，保留三位小数
  auc_with_ci_train <- sprintf("%.3f\n(%.3f-%.3f)", auc_train, ci_train[1], ci_train[3]) 
  
  ##计算训练队列的最佳阈值
  #定义阈值范围
  threshold <- seq(0, 1, by = 0.001)
  #对不同的阈值threshold计算性能指标
  metrics_list_train <- sapply(threshold, function(t) {
    calculate_metrics(pred_prob_train, true_class_train, t)
  }, simplify = F)
  
  distances <- sapply(metrics_list_train, function(metrics) {
    sqrt((1-metrics$Sensitivity)^2 + (1-metrics$Specificity)^2)
  })
  #找到训练队列的最佳阈值
  best_threshold_train <- threshold[which.min(distances)]
  
  #使用训练队列的最佳阈值计算训练队列的性能指标
  best_metrics_train <- calculate_metrics(pred_prob_train, true_class_train, best_threshold_train)
  
  #计算Brier score
  brier_score_train <- mean((pred_prob_train - true_class_train)^2)
  
  #Bootstrap CI
  set.seed(123)
  ci_train_metrics <- bootstrap_ci(pred_prob_train, true_class_train, best_threshold_train, B = 1000)
  
  Sensitivity_CI  <- format_ci(best_metrics_train$Sensitivity,  ci_train_metrics[, "Sensitivity"])
  Specificity_CI  <- format_ci(best_metrics_train$Specificity,  ci_train_metrics[, "Specificity"])
  PPV_CI          <- format_ci(best_metrics_train$PPV,          ci_train_metrics[, "PPV"])
  NPV_CI          <- format_ci(best_metrics_train$NPV,          ci_train_metrics[, "NPV"])
  CCR_CI          <- format_ci(best_metrics_train$CCR,          ci_train_metrics[, "CCR"])
  F1_score_CI     <- format_ci(best_metrics_train$F1_score,     ci_train_metrics[, "F1_score"])
  Brier_score_CI  <- format_ci(brier_score_train,               ci_train_metrics[, "Brier_score"])
  
  #汇总训练队列的模型结果
  train_metrics_result <- data.frame(
    Dataset_name = Dataset_name,
    AUC_CI = auc_with_ci_train,
    Threshold = round(best_threshold_train, 3),
    Sensitivity_CI = Sensitivity_CI,
    Specificity_CI = Specificity_CI,
    PPV_CI = PPV_CI,
    NPV_CI = NPV_CI,
    CCR_CI = CCR_CI,
    F1_score_CI = F1_score_CI,
    Brier_score_CI = Brier_score_CI
  )
  
  #返回结果
  return(train_metrics_result)
}

#测试队列模型性能评估函数
test_performance <- function(test_pred, test_data, train_threshold, Dataset_name) {
  #获取测试队列模型的预测概率
  pred_prob_test <- test_pred$prob[, "Yes"]
  
  #获取测试队列的实际分类
  true_class_test <- ifelse(test_data$CMDs == "Yes", 1, 0)
  
  roc_test <- roc(true_class_test, pred_prob_test)
  auc_test <- roc_test$auc
  ci_test <- ci(roc_test)
  #合并AUC和95%CI，保留三位小数
  auc_with_ci_test <- sprintf("%.3f\n(%.3f-%.3f)", auc_test, ci_test[1], ci_test[3]) 
  
  #使用训练队列的最佳阈值计算测试队列的性能指标
  best_metrics_test <- calculate_metrics(pred_prob_test, true_class_test, train_threshold)
  
  #计算Brier score
  brier_score_test <- mean((pred_prob_test - true_class_test)^2)
  
  #Bootstrap CI
  set.seed(123)
  ci_test_metrics <- bootstrap_ci(pred_prob_test, true_class_test, train_threshold, B = 1000)
  
  Sensitivity_CI  <- format_ci(best_metrics_test$Sensitivity,  ci_test_metrics[, "Sensitivity"])
  Specificity_CI  <- format_ci(best_metrics_test$Specificity,  ci_test_metrics[, "Specificity"])
  PPV_CI          <- format_ci(best_metrics_test$PPV,          ci_test_metrics[, "PPV"])
  NPV_CI          <- format_ci(best_metrics_test$NPV,          ci_test_metrics[, "NPV"])
  CCR_CI          <- format_ci(best_metrics_test$CCR,          ci_test_metrics[, "CCR"])
  F1_score_CI     <- format_ci(best_metrics_test$F1_score,     ci_test_metrics[, "F1_score"])
  Brier_score_CI  <- format_ci(brier_score_test,               ci_test_metrics[, "Brier_score"])
  
  #汇总测试队列的模型结果
  test_metrics_result <- data.frame(
    Dataset_name = Dataset_name,
    AUC_CI = auc_with_ci_test,
    Threshold = round(train_threshold, 3),
    Sensitivity_CI = Sensitivity_CI,
    Specificity_CI = Specificity_CI,
    PPV_CI = PPV_CI,
    NPV_CI = NPV_CI,
    CCR_CI = CCR_CI,
    F1_score_CI = F1_score_CI,
    Brier_score_CI = Brier_score_CI
  )
  
  #返回结果
  return(list(
    roc_test = roc_test,
    test_metrics_result = test_metrics_result
  ))
}

##------Low decline和High-stable组------
sensitivity1_LH_train_metrics <- train_performance(sensitivity1_LH_train_xgb_pred, 
                                                   sensitivity_data1_LH_train_std_num, 
                                                   Dataset_name = "sensitivity1_LH_train")

sensitivity1_LH_test_metrics <- test_performance(sensitivity1_LH_test_xgb_pred, 
                                                 sensitivity_data1_LH_test_std_num,
                                                 train_threshold = sensitivity1_LH_train_metrics$Threshold,
                                                 Dataset_name = "sensitivity1_LH_test")

#delong test
sensitivity1_LH_test_delong_test <- roc.test(LH_test_roc_auc_xgb$xgb_5, sensitivity1_LH_test_metrics[["roc_test"]], method = "delong")
#提取Z值和p-value，分别保留三位小数和自定义函数
sensitivity1_LH_test_Z_score <- round(sensitivity1_LH_test_delong_test$statistic, 3)
sensitivity1_LH_test_P_value <- pvalue_format(sensitivity1_LH_test_delong_test$p.value)


#------Unstable和High-stable组------
sensitivity1_UH_train_metrics <- train_performance(sensitivity1_UH_train_xgb_pred, 
                                                   sensitivity_data1_UH_train_std_num, 
                                                   Dataset_name = "sensitivity1_UH_train")

sensitivity1_UH_test_metrics <- test_performance(sensitivity1_UH_test_xgb_pred, 
                                                 sensitivity_data1_UH_test_std_num,
                                                 train_threshold = sensitivity1_UH_train_metrics$Threshold,
                                                 Dataset_name = "sensitivity1_UH_test")

#delong test
sensitivity1_UH_test_delong_test <- roc.test(UH_test_roc_auc_xgb$xgb_5, sensitivity1_UH_test_metrics[["roc_test"]], method = "delong")
#提取Z值和p-value，分别保留三位小数和自定义函数
sensitivity1_UH_test_Z_score <- round(sensitivity1_UH_test_delong_test$statistic, 3)
sensitivity1_UH_test_P_value <- pvalue_format(sensitivity1_UH_test_delong_test$p.value)

#将delong test结果添加到数据框并合并
sensitivity1_test_auc_comparisons <- data.frame(
  Model = "sensitivity1",
  LH_test_Z_score = sensitivity1_LH_test_Z_score,
  LH_test_P_value = sensitivity1_LH_test_P_value,
  UH_test_Z_score = sensitivity1_UH_test_Z_score,
  UH_test_P_value = sensitivity1_UH_test_P_value
)


##---------(2)敏感性分析2：多重插补---------
#生成默认插补方法模板
meth <- make.method(data_analyze2)

#变量类型划分函数
split_var_names <- function(df, max_cat_unique = 10, treat_int_small_unique_as_cat = TRUE) {
  continuous <- c()
  binary     <- c()
  multiclass <- c()
  other      <- c()
  
  for (col in names(df)) {
    x <- df[[col]]
    non_na <- x[!is.na(x)]
    n_unique <- length(unique(non_na))
    
    # 全缺失或常数列
    if (n_unique <= 1) {
      other <- c(other, col)
      next
    }
    
    # logical 直接当二分类
    if (is.logical(x)) {
      binary <- c(binary, col)
      next
    }
    
    # character / factor：按 unique=2 或 >2
    if (is.character(x) || is.factor(x)) {
      if (n_unique == 2) binary <- c(binary, col) else multiclass <- c(multiclass, col)
      next
    }
    
    # 数值型（integer/double）
    if (is.numeric(x)) {
      # 先判断二分类
      if (n_unique == 2) {
        binary <- c(binary, col)
        next
      }
      
      # 少量取值的“整数型”可视为多分类
      if (treat_int_small_unique_as_cat) {
        is_int_like <- all(abs(non_na - round(non_na)) < 1e-8)
        if (is_int_like && n_unique <= max_cat_unique) {
          multiclass <- c(multiclass, col)
          next
        }
      }
      
      # 否则连续变量
      continuous <- c(continuous, col)
      next
    }
    
    # 其他类型（Date, POSIXct, list 等）
    other <- c(other, col)
  }
  
  return(list(
    continuous = continuous,
    binary = binary,
    multiclass = multiclass,
    other = other
  ))
}

# 执行划分
var_types <- split_var_names(data_analyze2, max_cat_unique = 10)

#指定连续变量插补方法：pmm
meth[var_types$continuous] <- "pmm"

#指定二分类变量插补方法：logreg
meth[var_types$binary] <- "logreg"

#指定多分类变量插补方法：polyreg
meth[var_types$multiclass] <- "polyreg"

#执行多重插补
set.seed(126)
mi <- mice(data_analyze2, m = 5, method = meth, maxit = 20)

#提取第1套插补后的完整数据集
sensitivity_data2 <- complete(mi, action = 1)

###------划分数据集------
##轨迹组划分
#Low decline和High-stable组
sensitivity_data2_LH <- sensitivity_data2 %>%
  filter(eGFR_group %in% c(1, 3)) %>%
  select(-eGFR_group)  # 删除 eGFR_group 列

#Unstable和High-stable组
sensitivity_data2_UH <- sensitivity_data2 %>%
  filter(eGFR_group %in% c(2, 3)) %>%
  select(-eGFR_group)  # 删除 eGFR_group 列

##Low decline和High-stable组划分训练队列与测试队列
#设置随机种子，确保划分可重复性
set.seed(1217)
#createDataPartition()会自动从y的各个level随机取出等比例的数据来，p表示训练队列与测试队列占比，list = F表示返回的结果是一个向量，times = 1表示分割操作只执行一次 
sensitivity_data2_trainindex_LH <- createDataPartition(sensitivity_data2_LH$CMDs, p = 0.7, list = F, times = 1)
#划分出训练队列
sensitivity_data2_LH_train <- sensitivity_data2_LH[sensitivity_data2_trainindex_LH, ]
#划分出测试队列
sensitivity_data2_LH_test <- sensitivity_data2_LH[-sensitivity_data2_trainindex_LH, ]

##Unstable和High-stable组划分训练队列与测试队列
#设置随机种子，确保划分可重复性
set.seed(1217)
#createDataPartition()会自动从y的各个level随机取出等比例的数据来，p表示训练队列与测试队列占比，list = F表示返回的结果是一个向量，times = 1表示分割操作只执行一次 
sensitivity_data2_trainindex_UH <- createDataPartition(sensitivity_data2_UH$CMDs, p = 0.7, list = F, times = 1)
#划分出训练队列
sensitivity_data2_UH_train <- sensitivity_data2_UH[sensitivity_data2_trainindex_UH, ]
#划分出测试队列
sensitivity_data2_UH_test <- sensitivity_data2_UH[-sensitivity_data2_trainindex_UH, ]

###-----------变量标准化-----------
##Low decline和High-stable组变量标准化
#使用preProcess()对训练队列进行标准化处理（中心化和缩放），使得每列数据的均值为0，标准差为1
sensitivity_data2_std_para_LH <- preProcess(sensitivity_data2_LH_train, method = c("center", "scale"))
#对训练队列进行标准化
sensitivity_data2_LH_train_std <- predict(sensitivity_data2_std_para_LH, newdata = sensitivity_data2_LH_train)
#对测试队列进行标准化，使用训练队列的标准化参数
sensitivity_data2_LH_test_std <- predict(sensitivity_data2_std_para_LH, newdata = sensitivity_data2_LH_test)

##Unstable和High-stable组变量标准化
#使用preProcess()对训练队列进行标准化处理（中心化和缩放），使得每列数据的均值为0，标准差为1
sensitivity_data2_std_para_UH <- preProcess(sensitivity_data2_UH_train, method = c("center", "scale"))
#对训练队列进行标准化
sensitivity_data2_UH_train_std <- predict(sensitivity_data2_std_para_UH, newdata = sensitivity_data2_UH_train)
#对测试队列进行标准化，使用训练队列的标准化参数
sensitivity_data2_UH_test_std <- predict(sensitivity_data2_std_para_UH, newdata = sensitivity_data2_UH_test)

##XGBoost、SVM不能处理因子变量，数据集转换为数值型，除了CMDs
sensitivity_data2_LH_train_std_num <- to_num_except_CMDs(sensitivity_data2_LH_train_std)
sensitivity_data2_LH_test_std_num <- to_num_except_CMDs(sensitivity_data2_LH_test_std)
sensitivity_data2_UH_train_std_num <- to_num_except_CMDs(sensitivity_data2_UH_train_std)
sensitivity_data2_UH_test_std_num <- to_num_except_CMDs(sensitivity_data2_UH_test_std)

#提取选中的特征数据和结局数据
sensitivity_data2_LH5_train <- sensitivity_data2_LH_train_std_num[, c(LH_round_outputs[[7]][["features"]], "CMDs")]
sensitivity_data2_LH5_test <- sensitivity_data2_LH_test_std_num[, c(LH_round_outputs[[7]][["features"]], "CMDs")]
sensitivity_data2_UH5_train <- sensitivity_data2_UH_train_std_num[, c(UH_round_outputs[[7]][["features"]], "CMDs")]
sensitivity_data2_UH5_test <- sensitivity_data2_UH_test_std_num[, c(UH_round_outputs[[7]][["features"]], "CMDs")]

##------构建预测模型------
#创建任务
sensitivity2_LH_train_task <- as_task_classif(sensitivity_data2_LH5_train, target = "CMDs")
sensitivity2_LH_test_task <- as_task_classif(sensitivity_data2_LH5_test, target = "CMDs")
sensitivity2_UH_train_task <- as_task_classif(sensitivity_data2_UH5_train, target = "CMDs")
sensitivity2_UH_test_task <- as_task_classif(sensitivity_data2_UH5_test, target = "CMDs")

##训练、预测
#Low decline和High-stable组：XGBoost
sensitivity2_LH_xgb_learner <- lrn("classif.xgboost", predict_type = "prob", verbose = 0)
sensitivity2_LH_xgb_learner$param_set$values <- list(
  nrounds = to_tune(500, 1000), #树的数量：控制迭代次数，决定模型学习的程度。
  max_depth = to_tune(1, 10), #树的最大深度：控制树的复杂度，防止过拟合。
  eta = to_tune(0.001, 0.05), #学习率：控制学习的速度，影响收敛和稳定性。
  min_child_weight = to_tune(1, 10), #每个叶节点最小实例权重：限制分裂，防止过拟合。
  subsample = to_tune(0.1, 0.5) #子样本比例：提供随机性，减少过拟合风险，增加多样性。
)
set.seed(123)
sensitivity2_LH_xgb <- tune(tuner = tnr("grid_search", resolution = 5),
                            task = sensitivity2_LH_train_task,
                            learner = sensitivity2_LH_xgb_learner,
                            resampling = rsmp("cv", folds = 5),
                            measure = msr("classif.auc")
)
sensitivity2_LH_xgb_learner$param_set$values <- sensitivity2_LH_xgb$result_learner_param_vals
sensitivity2_LH_xgb_learner$train(sensitivity2_LH_train_task)
sensitivity2_LH_train_xgb_pred <- sensitivity2_LH_xgb_learner$predict(sensitivity2_LH_train_task)
sensitivity2_LH_test_xgb_pred <- sensitivity2_LH_xgb_learner$predict(sensitivity2_LH_test_task)

#Unstable和High-stable组：XGBoost
sensitivity2_UH_xgb_learner <- lrn("classif.xgboost", predict_type = "prob", verbose = 0)
sensitivity2_UH_xgb_learner$param_set$values <- list(
  nrounds = to_tune(100, 500), #树的数量：控制迭代次数，决定模型学习的程度。
  max_depth = to_tune(1, 10), #树的最大深度：控制树的复杂度，防止过拟合。
  eta = to_tune(0.001, 0.05), #学习率：控制学习的速度，影响收敛和稳定性。
  min_child_weight = to_tune(1, 10), #每个叶节点最小实例权重：限制分裂，防止过拟合。
  subsample = to_tune(0.5, 1) #子样本比例：提供随机性，减少过拟合风险，增加多样性。
)
set.seed(123)
sensitivity2_UH_xgb <- tune(tuner = tnr("grid_search", resolution = 5),
                            task = sensitivity2_UH_train_task,
                            learner = sensitivity2_UH_xgb_learner,
                            resampling = rsmp("cv", folds = 5),
                            measure = msr("classif.auc")
)
sensitivity2_UH_xgb_learner$param_set$values <- sensitivity2_UH_xgb$result_learner_param_vals
sensitivity2_UH_xgb_learner$train(sensitivity2_UH_train_task)
sensitivity2_UH_train_xgb_pred <- sensitivity2_UH_xgb_learner$predict(sensitivity2_UH_train_task)
sensitivity2_UH_test_xgb_pred <- sensitivity2_UH_xgb_learner$predict(sensitivity2_UH_test_task)
save(sensitivity2_UH_train_xgb_pred,
     sensitivity2_UH_test_xgb_pred,
     file = "C:/Users/74077/Desktop/sensitivity2_UH5_xgb_pred_eGFR4.RData")

###------性能评估------
##------Low decline和High-stable组------
sensitivity2_LH_train_metrics <- train_performance(sensitivity2_LH_train_xgb_pred, 
                                                   sensitivity_data2_LH_train_std_num, 
                                                   Dataset_name = "sensitivity2_LH_train")

sensitivity2_LH_test_metrics <- test_performance(sensitivity2_LH_test_xgb_pred, 
                                                 sensitivity_data2_LH_test_std_num,
                                                 train_threshold = sensitivity2_LH_train_metrics$Threshold,
                                                 Dataset_name = "sensitivity2_LH_test")

#delong test
sensitivity2_LH_test_delong_test <- roc.test(LH_test_roc_auc_xgb$xgb_5, sensitivity2_LH_test_metrics[["roc_test"]], method = "delong")
#提取Z值和p-value，分别保留三位小数和自定义函数
sensitivity2_LH_test_Z_score <- round(sensitivity2_LH_test_delong_test$statistic, 3)
sensitivity2_LH_test_P_value <- pvalue_format(sensitivity2_LH_test_delong_test$p.value)


#------Unstable和High-stable组------
sensitivity2_UH_train_metrics <- train_performance(sensitivity2_UH_train_xgb_pred, 
                                                   sensitivity_data2_UH_train_std_num, 
                                                   Dataset_name = "sensitivity2_UH_train")

sensitivity2_UH_test_metrics <- test_performance(sensitivity2_UH_test_xgb_pred, 
                                                 sensitivity_data2_UH_test_std_num,
                                                 train_threshold = sensitivity2_UH_train_metrics$Threshold,
                                                 Dataset_name = "sensitivity2_UH_test")

#delong test
sensitivity2_UH_test_delong_test <- roc.test(UH_test_roc_auc_xgb$xgb_5, sensitivity2_UH_test_metrics[["roc_test"]], method = "delong")
#提取Z值和p-value，分别保留三位小数和自定义函数
sensitivity2_UH_test_Z_score <- round(sensitivity2_UH_test_delong_test$statistic, 3)
sensitivity2_UH_test_P_value <- pvalue_format(sensitivity2_UH_test_delong_test$p.value)

#将delong test结果添加到数据框并合并
sensitivity2_test_auc_comparisons <- data.frame(
  Model = "sensitivity2",
  LH_test_Z_score = sensitivity2_LH_test_Z_score,
  LH_test_P_value = sensitivity2_LH_test_P_value,
  UH_test_Z_score = sensitivity2_UH_test_Z_score,
  UH_test_P_value = sensitivity2_UH_test_P_value
)

##---------(3)敏感性分析3：SMOTENC---------
#提取最终数据
LH5_train_data <- data_LH_train_std_num[, c(LH_round_outputs[[7]][["features"]], "CMDs")]
LH5_test_data <- data_LH_test_std_num[, c(LH_round_outputs[[7]][["features"]], "CMDs")]
UH5_train_data <- data_UH_train_std_num[, c(UH_round_outputs[[7]][["features"]], "CMDs")]
UH5_test_data <- data_UH_test_std_num[, c(UH_round_outputs[[7]][["features"]], "CMDs")]

###-----------不平衡处理：SMOTENC-----------
##LH5_train_data不平衡处理
#定义预处理配方recipe
rec_LH <- recipe(CMDs ~ ., data = LH5_train_data)
#设置随机种子，确保SMOTENC结果可重复性
set.seed(131)
#执行基于SMOTENC方法来处理类别不平衡问题
LH5_train_smotenc <- rec_LH %>% 
  step_smotenc(CMDs, over_ratio = 1) %>% #1:1平衡
  prep() %>% #将配方应用到训练数据上，完成增广的预处理步骤
  bake(new_data = NULL) #输出处理后的数据集

##UH5_train_data不平衡处理
#定义预处理配方recipe
rec_UH <- recipe(CMDs ~ ., data = UH5_train_data)
#设置随机种子，确保SMOTENC结果可重复性
set.seed(131)
#执行基于SMOTENC方法来处理类别不平衡问题
UH5_train_smotenc <- rec_UH %>% 
  step_smotenc(CMDs, over_ratio = 1) %>% #1:1平衡
  prep() %>% #将配方应用到训练数据上，完成增广的预处理步骤
  bake(new_data = NULL) #输出处理后的数据集


##-----构建预测模型: Low decline和High-stable组-----
#创建训练队列任务
LH5_train_task <- as_task_classif(LH5_train_smotenc, target = "CMDs")
#创建测试队列任务
LH5_test_task <- as_task_classif(LH5_test_data, target = "CMDs")

##XGBoost
LH5_xgb_learner <- lrn("classif.xgboost", predict_type = "prob")
LH5_xgb_learner$param_set$values <- list(
  nrounds = to_tune(500, 1000), #树的数量：控制迭代次数，决定模型学习的程度。
  max_depth = to_tune(1, 10), #树的最大深度：控制树的复杂度，防止过拟合。
  eta = to_tune(0.001, 0.05), #学习率：控制学习的速度，影响收敛和稳定性。
  min_child_weight = to_tune(1, 10), #每个叶节点最小实例权重：限制分裂，防止过拟合。
  subsample = to_tune(0.1, 0.5) #子样本比例：提供随机性，减少过拟合风险，增加多样性。
)
set.seed(123)
LH5_xgb <- tune(tuner = tnr("grid_search", resolution = 5),
                task = LH5_train_task,
                learner = LH5_xgb_learner,
                resampling = rsmp("cv", folds = 5),
                measure = msr("classif.auc")
)
LH5_xgb_learner$param_set$values <- LH5_xgb$result_learner_param_vals
LH5_xgb_learner$train(LH5_train_task)
LH5_train_xgb_pred <- LH5_xgb_learner$predict(LH5_train_task)
LH5_test_xgb_pred <- LH5_xgb_learner$predict(LH5_test_task)


##-----构建预测模型: Unstable和High-stable组-----
#创建训练队列任务
UH5_train_task <- as_task_classif(UH5_train_smotenc, target = "CMDs")
#创建测试队列任务
UH5_test_task <- as_task_classif(UH5_test_data, target = "CMDs")

##XGBoost
UH5_xgb_learner <- lrn("classif.xgboost", predict_type = "prob")
UH5_xgb_learner$param_set$values <- list(
  nrounds = to_tune(100, 500), #树的数量：控制迭代次数，决定模型学习的程度。
  max_depth = to_tune(1, 10), #树的最大深度：控制树的复杂度，防止过拟合。
  eta = to_tune(0.001, 0.05), #学习率：控制学习的速度，影响收敛和稳定性。
  min_child_weight = to_tune(1, 10), #每个叶节点最小实例权重：限制分裂，防止过拟合。
  subsample = to_tune(0.5, 1) #子样本比例：提供随机性，减少过拟合风险，增加多样性。
)
set.seed(123)
UH5_xgb <- tune(tuner = tnr("grid_search", resolution = 5),
                task = UH5_train_task,
                learner = UH5_xgb_learner,
                resampling = rsmp("cv", folds = 5),
                measure = msr("classif.auc")
)
UH5_xgb_learner$param_set$values <- UH5_xgb$result_learner_param_vals
UH5_xgb_learner$train(UH5_train_task)
UH5_train_xgb_pred <- UH5_xgb_learner$predict(UH5_train_task)
UH5_test_xgb_pred <- UH5_xgb_learner$predict(UH5_test_task)


###------性能评估------
##------Low decline和High-stable组------
sensitivity3_LH_train_metrics <- train_performance(LH5_train_xgb_pred, 
                                                   LH5_train_smotenc, 
                                                   Dataset_name = "sensitivity3_LH_train")

sensitivity3_LH_test_metrics <- test_performance(LH5_test_xgb_pred, 
                                                 LH5_test_data,
                                                 train_threshold = sensitivity3_LH_train_metrics$Threshold,
                                                 Dataset_name = "sensitivity3_LH_test")

#delong test
sensitivity3_LH_test_delong_test <- roc.test(LH_test_roc_auc_xgb$xgb_5, sensitivity3_LH_test_metrics[["roc_test"]], method = "delong")
#提取Z值和p-value，分别保留三位小数和自定义函数
sensitivity3_LH_test_Z_score <- round(sensitivity3_LH_test_delong_test$statistic, 3)
sensitivity3_LH_test_P_value <- pvalue_format(sensitivity3_LH_test_delong_test$p.value)


#------Unstable和High-stable组------
sensitivity3_UH_train_metrics <- train_performance(UH5_train_xgb_pred, 
                                                   UH5_train_smotenc, 
                                                   Dataset_name = "sensitivity3_UH_train")

sensitivity3_UH_test_metrics <- test_performance(UH5_test_xgb_pred, 
                                                 UH5_test_data,
                                                 train_threshold = sensitivity3_UH_train_metrics$Threshold,
                                                 Dataset_name = "sensitivity3_UH_test")

#delong test
sensitivity3_UH_test_delong_test <- roc.test(UH_test_roc_auc_xgb$xgb_5, sensitivity3_UH_test_metrics[["roc_test"]], method = "delong")
#提取Z值和p-value，分别保留三位小数和自定义函数
sensitivity3_UH_test_Z_score <- round(sensitivity3_UH_test_delong_test$statistic, 3)
sensitivity3_UH_test_P_value <- pvalue_format(sensitivity3_UH_test_delong_test$p.value)

#将delong test结果添加到数据框并合并
sensitivity3_test_auc_comparisons <- data.frame(
  Model = "sensitivity3",
  LH_test_Z_score = sensitivity3_LH_test_Z_score,
  LH_test_P_value = sensitivity3_LH_test_P_value,
  UH_test_Z_score = sensitivity3_UH_test_Z_score,
  UH_test_P_value = sensitivity3_UH_test_P_value
)


#合并3个敏感性分析delong test结果
sensitivity123_delong_result <- rbind(sensitivity1_test_auc_comparisons, 
                                      sensitivity2_test_auc_comparisons,
                                      sensitivity3_test_auc_comparisons)

#合并3个敏感性分析各性能指标结果
sensitivity123_result <- rbind(sensitivity1_LH_test_metrics[["test_metrics_result"]], 
                               sensitivity2_LH_test_metrics[["test_metrics_result"]], 
                               sensitivity3_LH_test_metrics[["test_metrics_result"]], 
                               sensitivity1_UH_test_metrics[["test_metrics_result"]],
                               sensitivity2_UH_test_metrics[["test_metrics_result"]],
                               sensitivity3_UH_test_metrics[["test_metrics_result"]])

###------敏感性分析SHAP摘要图------
#敏感性分析SHAP对象函数
sensitivity_shap_sv <- function(sensitivity_train_task, 
                                sensitivity_test_task, 
                                sensitivity_learner) {
  #提取训练队列和测试队列预测变量
  sensitivity_train_x <- sensitivity_train_task$data(cols = sensitivity_train_task$feature_names)
  sensitivity_test_x <- sensitivity_test_task$data(cols = sensitivity_test_task$feature_names)
  #随机抽取训练队列200行
  set.seed(123)
  sensitivity_train_x_200 <- sensitivity_train_x %>% 
    slice_sample(n = 200)
  
  #计算SHAP值：svm_learner训练好的模型；X = test_x需要解释的测试数据集；bg_X = train_x_200：背景数据集，用于计算 SHAP 值的基准；predict_type = "prob"：指定模型的预测类型为概率；verbose = F关闭详细输出。
  shap_value <- kernelshap(sensitivity_learner, 
                           X = sensitivity_test_x, 
                           bg_X = sensitivity_train_x_200, predict_type = "prob", verbose = T)
  
  #构建可视化对象
  sv_xgb <- shapviz(shap_value, which_class = 2) 
  #数据字典映射变量名
  shap_values_df <- as.data.frame(sv_xgb$S)
  colnames(shap_values_df) <- variable_dict$display_name[match(names(shap_values_df), variable_dict$analysis_name)]
  sv_xgb$S <- as.matrix(shap_values_df)
  colnames(sv_xgb$X) <- variable_dict$display_name[match(names(sv_xgb$X), variable_dict$analysis_name)]
  
  return(sv_xgb)
}

#调用函数做各敏感性分析SHAP对象
LH_sensitivity1_shap_sv <- sensitivity_shap_sv(sensitivity1_LH_train_task, 
                                               sensitivity1_LH_test_task, 
                                               sensitivity1_LH_xgb_learner) 
LH_sensitivity2_shap_sv <- sensitivity_shap_sv(sensitivity2_LH_train_task, 
                                               sensitivity2_LH_test_task, 
                                               sensitivity2_LH_xgb_learner) 
LH_sensitivity3_shap_sv <- sensitivity_shap_sv(LH5_train_task, 
                                               LH5_test_task, 
                                               LH5_xgb_learner) 

UH_sensitivity1_shap_sv <- sensitivity_shap_sv(sensitivity1_UH_train_task, 
                                               sensitivity1_UH_test_task, 
                                               sensitivity1_UH_xgb_learner) 
UH_sensitivity2_shap_sv <- sensitivity_shap_sv(sensitivity2_UH_train_task, 
                                               sensitivity2_UH_test_task, 
                                               sensitivity2_UH_xgb_learner) 
UH_sensitivity3_shap_sv <- sensitivity_shap_sv(UH5_train_task, 
                                               UH5_test_task, 
                                               UH5_xgb_learner) 

#SHAP蜂群图
LH_sensitivity1_shap_beeswarm <- sv_importance(LH_sensitivity1_shap_sv, 
                                               kind = "beeswarm", #蜂群图
                                               size = 0.2, # 散点大小（蜂群图）
                                               bee_width = 0.3, #散点的水平扩展宽度
                                               max_display = Inf) + #显示所有特征
  scale_color_gradient(low = "#6601F7", high = "#48EDFE") + 
  scale_x_continuous(limits = c(-0.1, 0.5), #限制x轴刻度
                     breaks = seq(-0.1, 0.5, by = 0.1)) + #设置x轴刻度
  theme(
    panel.background = element_rect(fill = "white"), #面板背景为白色
    plot.background = element_rect(fill = "white"), #整个绘图区域背景为白色
    axis.title = element_text(size = 40), #坐标轴标题字体大小
    axis.text = element_text(size = 30), #坐标轴刻度字体大小
    legend.title = element_text(size = 30), #图例标题字体大小
    legend.text = element_text(size = 20), #图例字体大小
    legend.key.height = unit(0.35, "cm"), #条形图例高度
    legend.key.width = unit(0.1, "cm"), #条形图例宽度
    plot.tag = element_text(size = 70, face = "bold"), #设置标签文字大小
    plot.margin = margin(0, 0, 0, 0, "cm"), #调整边距
    axis.ticks = element_line(size = 0.25), #y轴刻度线粗细
    axis.ticks.length = unit(0.05, "cm"), #刻度线在外侧及长度
    axis.ticks.y = element_blank() #去掉y轴刻度线
  )

LH_sensitivity2_shap_beeswarm <- sv_importance(LH_sensitivity2_shap_sv, 
                                               kind = "beeswarm", #蜂群图
                                               size = 0.2, # 散点大小（蜂群图）
                                               bee_width = 0.3, #散点的水平扩展宽度
                                               max_display = Inf) + #显示所有特征
  scale_color_gradient(low = "#6601F7", high = "#48EDFE") + 
  scale_x_continuous(limits = c(-0.1, 0.7), #限制x轴刻度
                     breaks = seq(-0.1, 0.7, by = 0.1)) + #设置x轴刻度
  theme(
    panel.background = element_rect(fill = "white"), #面板背景为白色
    plot.background = element_rect(fill = "white"), #整个绘图区域背景为白色
    axis.title = element_text(size = 40), #坐标轴标题字体大小
    axis.text = element_text(size = 30), #坐标轴刻度字体大小
    legend.title = element_text(size = 30), #图例标题字体大小
    legend.text = element_text(size = 20), #图例字体大小
    legend.key.height = unit(0.35, "cm"), #条形图例高度
    legend.key.width = unit(0.1, "cm"), #条形图例宽度
    plot.tag = element_text(size = 70, face = "bold"), #设置标签文字大小
    plot.margin = margin(0, 0, 0, 0, "cm"), #调整边距
    axis.ticks = element_line(size = 0.25), #y轴刻度线粗细
    axis.ticks.length = unit(0.05, "cm"), #刻度线在外侧及长度
    axis.ticks.y = element_blank() #去掉y轴刻度线
  )

LH_sensitivity3_shap_beeswarm <- sv_importance(LH_sensitivity3_shap_sv, 
                                               kind = "beeswarm", #蜂群图
                                               size = 0.2, # 散点大小（蜂群图）
                                               bee_width = 0.3, #散点的水平扩展宽度
                                               max_display = Inf) + #显示所有特征
  scale_color_gradient(low = "#6601F7", high = "#48EDFE") + 
  scale_x_continuous(limits = c(-0.6, 0.7), #限制x轴刻度
                     breaks = seq(-0.6, 0.7, by = 0.3)) + #设置x轴刻度
  theme(
    panel.background = element_rect(fill = "white"), #面板背景为白色
    plot.background = element_rect(fill = "white"), #整个绘图区域背景为白色
    axis.title = element_text(size = 40), #坐标轴标题字体大小
    axis.text = element_text(size = 30), #坐标轴刻度字体大小
    legend.title = element_text(size = 30), #图例标题字体大小
    legend.text = element_text(size = 20), #图例字体大小
    legend.key.height = unit(0.35, "cm"), #条形图例高度
    legend.key.width = unit(0.1, "cm"), #条形图例宽度
    plot.tag = element_text(size = 70, face = "bold"), #设置标签文字大小
    plot.margin = margin(0, 0, 0, 0, "cm"), #调整边距
    axis.ticks = element_line(size = 0.25), #y轴刻度线粗细
    axis.ticks.length = unit(0.05, "cm"), #刻度线在外侧及长度
    axis.ticks.y = element_blank() #去掉y轴刻度线
  )

UH_sensitivity1_shap_beeswarm <- sv_importance(UH_sensitivity1_shap_sv, 
                                               kind = "beeswarm", #蜂群图
                                               size = 0.2, # 散点大小（蜂群图）
                                               bee_width = 0.3, #散点的水平扩展宽度
                                               max_display = Inf) + #显示所有特征
  scale_color_gradient(low = "#6601F7", high = "#48EDFE") + 
  scale_x_continuous(limits = c(-0.1, 0.5), #限制x轴刻度
                     breaks = seq(-0.1, 0.5, by = 0.1)) + #设置x轴刻度
  theme(
    panel.background = element_rect(fill = "white"), #面板背景为白色
    plot.background = element_rect(fill = "white"), #整个绘图区域背景为白色
    axis.title = element_text(size = 40), #坐标轴标题字体大小
    axis.text = element_text(size = 30), #坐标轴刻度字体大小
    legend.title = element_text(size = 30), #图例标题字体大小
    legend.text = element_text(size = 20), #图例字体大小
    legend.key.height = unit(0.35, "cm"), #条形图例高度
    legend.key.width = unit(0.1, "cm"), #条形图例宽度
    plot.tag = element_text(size = 70, face = "bold"), #设置标签文字大小
    plot.margin = margin(0, 0, 0, 0, "cm"), #调整边距
    axis.ticks = element_line(size = 0.25), #y轴刻度线粗细
    axis.ticks.length = unit(0.05, "cm"), #刻度线在外侧及长度
    axis.ticks.y = element_blank() #去掉y轴刻度线
  )

UH_sensitivity2_shap_beeswarm <- sv_importance(UH_sensitivity2_shap_sv, 
                                               kind = "beeswarm", #蜂群图
                                               size = 0.2, # 散点大小（蜂群图）
                                               bee_width = 0.3, #散点的水平扩展宽度
                                               max_display = Inf) + #显示所有特征
  scale_color_gradient(low = "#6601F7", high = "#48EDFE") + 
  scale_x_continuous(limits = c(-0.1, 0.5), #限制x轴刻度
                     breaks = seq(-0.1, 0.5, by = 0.1)) + #设置x轴刻度
  theme(
    panel.background = element_rect(fill = "white"), #面板背景为白色
    plot.background = element_rect(fill = "white"), #整个绘图区域背景为白色
    axis.title = element_text(size = 40), #坐标轴标题字体大小
    axis.text = element_text(size = 30), #坐标轴刻度字体大小
    legend.title = element_text(size = 30), #图例标题字体大小
    legend.text = element_text(size = 20), #图例字体大小
    legend.key.height = unit(0.35, "cm"), #条形图例高度
    legend.key.width = unit(0.1, "cm"), #条形图例宽度
    plot.tag = element_text(size = 70, face = "bold"), #设置标签文字大小
    plot.margin = margin(0, 0, 0, 0, "cm"), #调整边距
    axis.ticks = element_line(size = 0.25), #y轴刻度线粗细
    axis.ticks.length = unit(0.05, "cm"), #刻度线在外侧及长度
    axis.ticks.y = element_blank() #去掉y轴刻度线
  )

UH_sensitivity3_shap_beeswarm <- sv_importance(UH_sensitivity3_shap_sv, 
                                               kind = "beeswarm", #蜂群图
                                               size = 0.2, # 散点大小（蜂群图）
                                               bee_width = 0.3, #散点的水平扩展宽度
                                               max_display = Inf) + #显示所有特征
  scale_color_gradient(low = "#6601F7", high = "#48EDFE") + 
  scale_x_continuous(limits = c(-0.6, 0.6), #限制x轴刻度
                     breaks = seq(-0.6, 0.6, by = 0.3)) + #设置x轴刻度
  theme(
    panel.background = element_rect(fill = "white"), #面板背景为白色
    plot.background = element_rect(fill = "white"), #整个绘图区域背景为白色
    axis.title = element_text(size = 40), #坐标轴标题字体大小
    axis.text = element_text(size = 30), #坐标轴刻度字体大小
    legend.title = element_text(size = 30), #图例标题字体大小
    legend.text = element_text(size = 20), #图例字体大小
    legend.key.height = unit(0.35, "cm"), #条形图例高度
    legend.key.width = unit(0.1, "cm"), #条形图例宽度
    plot.tag = element_text(size = 70, face = "bold"), #设置标签文字大小
    plot.margin = margin(0, 0, 0, 0, "cm"), #调整边距
    axis.ticks = element_line(size = 0.25), #y轴刻度线粗细
    axis.ticks.length = unit(0.05, "cm"), #刻度线在外侧及长度
    axis.ticks.y = element_blank() #去掉y轴刻度线
  )

#合并3个敏感性分析SHAP摘要图
LH_UH_sensitivity_shap_beeswarm <- (LH_sensitivity1_shap_beeswarm + UH_sensitivity1_shap_beeswarm) /
  (LH_sensitivity2_shap_beeswarm + UH_sensitivity2_shap_beeswarm) /
  (LH_shap_beeswarm_eGFR4 + UH_shap_beeswarm_eGFR4) +
  plot_annotation(tag_levels = "A") #自动添加标签

#导出3个敏感性分析SHAP摘要图
ggsave(plot = LH_UH_sensitivity_shap_beeswarm, 
       filename = "C:/Users/74077/Desktop/LH_UH_sensitivity_shap_beeswarm.png", 
       width = 17, 
       height = 10.5,
       units = "cm", 
       dpi = 600)


##------------亚组分析：年龄、性别------------
#按 Age 拆分
LH_Age_list <- data_LH_test_std_num %>%
  split(ifelse(data_LH_test$Age < 60, "1", "2"))
UH_Age_list <- data_UH_test_std_num %>%
  split(ifelse(data_UH_test$Age < 60, "1", "2"))

#按 Sex 拆分
LH_Sex_list <- split(data_LH_test_std_num, data_LH_test_std_num$Sex)
UH_Sex_list <- split(data_UH_test_std_num, data_UH_test_std_num$Sex)

#任意分类任务计算性能函数
evaluate_task_metrics <- function(data, dataset_name = "dataset_name") {
  #提取最终预测因子和结局
  if (grepl("LH", dataset_name)) {
    subgroup_data <- data[, c(LH_round_outputs[[7]][["features"]], "CMDs")]
  } else {
    subgroup_data <- data[, c(UH_round_outputs[[7]][["features"]], "CMDs")]
  }
  
  #创建任务
  task <- as_task_classif(subgroup_data, target = "CMDs")
  
  # 根据 dataset_name 选择 learner
  if (grepl("LH", dataset_name)) {
    xgb_pred <- LH_round_outputs[[7]][["xgb_learner"]]$predict(task)
  } else {
    xgb_pred <- UH_round_outputs[[7]][["xgb_learner"]]$predict(task)
  }
  
  #创建预测结果列表
  preds_list <- list(XGBoost  = xgb_pred)
  
  #创建空的数据框，用于存储性能评估结果
  metrics_df <- data.frame()
  
  for (model_name in names(preds_list)) {
    pred_obj   <- preds_list[[model_name]]
    pred_prob  <- pred_obj$prob[, "Yes"]
    true_class <- pred_obj$truth
    true_class <- ifelse(true_class == "Yes", 1, 0)
    
    # 如果某个亚组里只有一种结局，ROC/AUC 算不了，直接跳过
    if (length(unique(true_class)) < 2) {
      next
    }
    
    roc_obj <- roc(true_class, pred_prob)
    auc_val <- roc_obj$auc
    ci_auc  <- ci(roc_obj)
    
    # 合并 AUC 和 95%CI
    auc_with_ci <- sprintf("%.3f\n(%.3f-%.3f)", auc_val, ci_auc[1], ci_auc[3]) 
    
    # 从训练队列记录中取该模型的最佳阈值
    # 根据 dataset_name 选择最佳阈值
    if (grepl("LH", dataset_name)) {
      best_threshold_train <- LH_train_metrics_xgb[LH_train_metrics_xgb$Model == "xgb_5", "Threshold"]
    } else {
      best_threshold_train <- UH_train_metrics_xgb[UH_train_metrics_xgb$Model == "xgb_5", "Threshold"]
    }
    
    # 用训练队列阈值计算该 task 上的性能
    best_metrics <- calculate_metrics(pred_prob, true_class, best_threshold_train)
    
    # Brier score
    brier_score <- mean((pred_prob - true_class)^2)
    
    # bootstrap CI
    set.seed(123)
    ci_boot <- bootstrap_ci(pred_prob, true_class, best_threshold_train, B = 1000)
    
    Sensitivity_CI <- format_ci(best_metrics$Sensitivity, ci_boot[, "Sensitivity"])
    Specificity_CI <- format_ci(best_metrics$Specificity, ci_boot[, "Specificity"])
    PPV_CI         <- format_ci(best_metrics$PPV,        ci_boot[, "PPV"])
    NPV_CI         <- format_ci(best_metrics$NPV,        ci_boot[, "NPV"])
    CCR_CI         <- format_ci(best_metrics$CCR,        ci_boot[, "CCR"])
    F1_score_CI    <- format_ci(best_metrics$F1_score,   ci_boot[, "F1_score"])
    Brier_score_CI <- format_ci(brier_score,             ci_boot[, "Brier_score"])
    
    one_row <- data.frame(
      Model          = model_name,
      Dataset        = dataset_name,
      AUC_CI         = auc_with_ci,
      Threshold      = best_threshold_train,
      Sensitivity_CI = Sensitivity_CI,
      Specificity_CI = Specificity_CI,
      PPV_CI         = PPV_CI,
      NPV_CI         = NPV_CI,
      CCR_CI         = CCR_CI,
      F1_score_CI    = F1_score_CI,
      Brier_score_CI = Brier_score_CI, 
      stringsAsFactors = FALSE
    )
    
    metrics_df <- rbind(metrics_df, one_row)
  }
  
  return(list(roc_obj = roc_obj, metrics_df = metrics_df))
}

##--------亚组性能评估--------
#各亚组性能
LH_Age1_metrics <- evaluate_task_metrics(LH_Age_list[["1"]], dataset_name = "LH_Age (45-59 years)")
LH_Age2_metrics <- evaluate_task_metrics(LH_Age_list[["2"]], dataset_name = "LH_Age (≥60 years)")
LH_Sex1_metrics <- evaluate_task_metrics(LH_Sex_list[["1"]], dataset_name = "LH_Sex (Man)")
LH_Sex2_metrics <- evaluate_task_metrics(LH_Sex_list[["2"]], dataset_name = "LH_Sex (Woman)")

UH_Age1_metrics <- evaluate_task_metrics(UH_Age_list[["1"]], dataset_name = "UH_Age (45-59 years)")
UH_Age2_metrics <- evaluate_task_metrics(UH_Age_list[["2"]], dataset_name = "UH_Age (≥60 years)")
UH_Sex1_metrics <- evaluate_task_metrics(UH_Sex_list[["1"]], dataset_name = "UH_Sex (Man)")
UH_Sex2_metrics <- evaluate_task_metrics(UH_Sex_list[["2"]], dataset_name = "UH_Sex (Woman)")

##delong test
#delong test——年龄组
LH_Age_delong_test <- roc.test(LH_Age1_metrics[["roc_obj"]], 
                               LH_Age2_metrics[["roc_obj"]], 
                               method = "delong")
#提取Z值和p-value，分别保留三位小数和自定义函数
LH_Age_Z_score <- round(LH_Age_delong_test$statistic, 3)
LH_Age_P_value <- pvalue_format(LH_Age_delong_test$p.value)

UH_Age_delong_test <- roc.test(UH_Age1_metrics[["roc_obj"]], 
                               UH_Age2_metrics[["roc_obj"]], 
                               method = "delong")
#提取Z值和p-value，分别保留三位小数和自定义函数
UH_Age_Z_score <- round(UH_Age_delong_test$statistic, 3)
UH_Age_P_value <- pvalue_format(UH_Age_delong_test$p.value)

#delong test——性别组
LH_Sex_delong_test <- roc.test(LH_Sex1_metrics[["roc_obj"]], 
                               LH_Sex2_metrics[["roc_obj"]], 
                               method = "delong")
#提取Z值和p-value，分别保留三位小数和自定义函数
LH_Sex_Z_score <- round(LH_Sex_delong_test$statistic, 3)
LH_Sex_P_value <- pvalue_format(LH_Sex_delong_test$p.value)

UH_Sex_delong_test <- roc.test(UH_Sex1_metrics[["roc_obj"]], 
                               UH_Sex2_metrics[["roc_obj"]], 
                               method = "delong")
#提取Z值和p-value，分别保留三位小数和自定义函数
UH_Sex_Z_score <- round(UH_Sex_delong_test$statistic, 3)
UH_Sex_P_value <- pvalue_format(UH_Sex_delong_test$p.value)

#将delong test结果添加到数据框
subgroup_auc_comparisons <- data.frame(
  Subgroup = c("LH_Age", "LH_Sex", "UH_Age", "UH_Sex"),
  Z_score = c(LH_Age_Z_score, LH_Sex_Z_score, UH_Age_Z_score, UH_Sex_Z_score),
  P_value = c(LH_Age_P_value, LH_Sex_P_value, UH_Age_P_value, UH_Sex_P_value)
)


#合并各亚组性能
subgroup_metrics_result <- rbind(LH_Age1_metrics[["metrics_df"]], 
                                 LH_Age2_metrics[["metrics_df"]], 
                                 LH_Sex1_metrics[["metrics_df"]], 
                                 LH_Sex2_metrics[["metrics_df"]],
                                 UH_Age1_metrics[["metrics_df"]], 
                                 UH_Age2_metrics[["metrics_df"]], 
                                 UH_Sex1_metrics[["metrics_df"]], 
                                 UH_Sex2_metrics[["metrics_df"]])


##--------亚组SHAP摘要图--------
#亚组SHAP函数
subgroup_shap <- function(train_data, test_data, group) {
  #提取最终预测因子和结局
  if (grepl("LH", group)) {
    train_data <- train_data[, c(LH_round_outputs[[7]][["features"]], "CMDs")]
  } else {
    train_data <- train_data[, c(UH_round_outputs[[7]][["features"]], "CMDs")]
  }
  
  if (grepl("LH", group)) {
    test_subgroup_data <- test_data[, c(LH_round_outputs[[7]][["features"]], "CMDs")]
  } else {
    test_subgroup_data <- test_data[, c(UH_round_outputs[[7]][["features"]], "CMDs")]
  }
  
  #创建任务
  train_task <- as_task_classif(train_data, target = "CMDs")
  test_subgroup_task <- as_task_classif(test_subgroup_data, target = "CMDs")
  
  #提取训练队列和测试队列预测变量
  train_x <- train_task$data(cols = train_task$feature_names)
  test_subgroup_x <- test_subgroup_task$data(cols = test_subgroup_task$feature_names)
  #随机抽取训练队列200行
  set.seed(123)
  train_x_200 <- train_x %>% 
    slice_sample(n = 200)
  
  #计算SHAP值：svm_learner训练好的模型；X = test_x需要解释的测试数据集；bg_X = train_x_200：背景数据集，用于计算 SHAP 值的基准；predict_type = "prob"：指定模型的预测类型为概率；verbose = F关闭详细输出。
  if (grepl("LH", group)) {
    shap_value <- kernelshap(LH_round_outputs[[7]][["xgb_learner"]], X = test_subgroup_x, bg_X = train_x_200, predict_type = "prob", verbose = T)
  } else {
    shap_value <- kernelshap(UH_round_outputs[[7]][["xgb_learner"]], X = test_subgroup_x, bg_X = train_x_200, predict_type = "prob", verbose = T)
  }
  #构建可视化对象
  sv_xgb <- shapviz(shap_value, which_class = 2) 
  #数据字典映射变量名
  shap_values_df <- as.data.frame(sv_xgb$S)
  colnames(shap_values_df) <- variable_dict$display_name[match(names(shap_values_df), variable_dict$analysis_name)]
  sv_xgb$S <- as.matrix(shap_values_df)
  colnames(sv_xgb$X) <- variable_dict$display_name[match(names(sv_xgb$X), variable_dict$analysis_name)]
  
  #SHAP蜂群图
  shap_beeswarm <- sv_importance(sv_xgb, 
                                 kind = "beeswarm", #蜂群图
                                 size = 0.2, # 散点大小（蜂群图）
                                 bee_width = 0.3, #散点的水平扩展宽度
                                 max_display = Inf) + #显示所有特征
    scale_color_gradient(low = "#6601F7", high = "#48EDFE") + 
    scale_x_continuous(limits = c(-0.2, 0.8), #限制x轴刻度
                       breaks = seq(-0.2, 0.8, by = 0.2)) + #设置x轴刻度
    theme(
      panel.background = element_rect(fill = "white"), #面板背景为白色
      plot.background = element_rect(fill = "white"), #整个绘图区域背景为白色
      axis.title = element_text(size = 40), #坐标轴标题字体大小
      axis.text = element_text(size = 30), #坐标轴刻度字体大小
      legend.title = element_text(size = 30), #图例标题字体大小
      legend.text = element_text(size = 20), #图例字体大小
      legend.key.height = unit(0.35, "cm"), #条形图例高度
      legend.key.width = unit(0.1, "cm"), #条形图例宽度
      plot.tag = element_text(size = 70, face = "bold"), #设置标签文字大小
      plot.margin = margin(0, 0, 0, 0, "cm"), #调整边距
      axis.ticks = element_line(size = 0.25), #y轴刻度线粗细
      axis.ticks.length = unit(0.05, "cm"), #刻度线在外侧及长度
      axis.ticks.y = element_blank() #去掉y轴刻度线
    )
  
  return(list(
    sv_xgb   = sv_xgb,
    shap_beeswarm = shap_beeswarm
  ))
}

#调用函数做各亚组SHAP图
LH_Age1_shap <- subgroup_shap(data_LH_train_std_num, LH_Age_list[["1"]], group = "LH")  # 45–59 岁
LH_Age2_shap <- subgroup_shap(data_LH_train_std_num, LH_Age_list[["2"]], group = "LH")  # >=60 岁
LH_Sex1_shap <- subgroup_shap(data_LH_train_std_num, LH_Sex_list[["1"]], group = "LH")  # Man
LH_Sex2_shap <- subgroup_shap(data_LH_train_std_num, LH_Sex_list[["2"]], group = "LH")  # Woman

UH_Age1_shap <- subgroup_shap(data_UH_train_std_num, UH_Age_list[["1"]], group = "UH")  # 45–59 岁
UH_Age2_shap <- subgroup_shap(data_UH_train_std_num, UH_Age_list[["2"]], group = "UH")  # >=60 岁
UH_Sex1_shap <- subgroup_shap(data_UH_train_std_num, UH_Sex_list[["1"]], group = "UH")  # Man
UH_Sex2_shap <- subgroup_shap(data_UH_train_std_num, UH_Sex_list[["2"]], group = "UH")  # Woman

LH_subgroup_shap <- (LH_Age1_shap[["shap_beeswarm"]] + LH_Age2_shap[["shap_beeswarm"]]) / 
  (LH_Sex1_shap[["shap_beeswarm"]] + LH_Sex2_shap[["shap_beeswarm"]]) + 
  plot_annotation(tag_levels = "A") #自动添加标签

#导出LH组SHAP摘要图
ggsave(plot = LH_subgroup_shap, 
       filename = "LH_subgroup_shap.png", 
       width = 17, 
       height = 7,
       units = "cm", 
       dpi = 600)

UH_subgroup_shap <- (UH_Age1_shap[["shap_beeswarm"]] + UH_Age2_shap[["shap_beeswarm"]]) / 
  (UH_Sex1_shap[["shap_beeswarm"]] + UH_Sex2_shap[["shap_beeswarm"]]) + 
  plot_annotation(tag_levels = "A") #自动添加标签

#导出UH组SHAP摘要图
ggsave(plot = UH_subgroup_shap, 
       filename = "UH_subgroup_shap.png", 
       width = 17, 
       height = 7,
       units = "cm", 
       dpi = 600)



##------------------导出word------------------
#创建一个空白的 Word 文档
doc <- read_docx()
#将数据框转换为flextable格式
TableS1 <- flextable(baseline)
#将表格添加到Word文档中
doc <- body_add_par(doc, value = "Table test_metrics_eGFR") #表格之间加一个空行，进行分割
doc <- body_add_flextable(doc, value = TableS1)
#导出Word文档
print(doc, target = "C:/Users/74077/Desktop/sensitivity123_result.docx")

save(pred_dfci_eGFR4,
     file = "C:/Users/74077/Desktop/pred_dfci_eGFR4.RData")
