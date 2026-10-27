# =====================================================================
#  R Math Solver — решатель математических задач на R
#  Три блока: финансовая математика, производные, статистика
#  (по 10 задач в каждом). Приложение на Shiny; на GitHub Pages
#  работает прямо в браузере через shinylive (webR).
# =====================================================================

library(shiny)
library(bslib)

# ---------------------------------------------------------------------
#  Вспомогательные функции
# ---------------------------------------------------------------------

# число -> строка для вставки в R-код
n2s <- function(x) format(x, scientific = FALSE, digits = 15, trim = TRUE)
vec2s <- function(x) paste0("c(", paste(n2s(x), collapse = ", "), ")")

# красивое форматирование результата
fm <- function(x, d = 2) formatC(x, format = "f", digits = d, big.mark = " ")
rub <- function(x) paste(fm(x), "₽")
pct <- function(x, d = 2) paste0(fm(x * 100, d), "%")

# подстановка значений в шаблон кода: {name} -> value
tpl <- function(s, ...) {
  vals <- list(...)
  for (n in names(vals)) s <- gsub(paste0("{", n, "}"), vals[[n]], s, fixed = TRUE)
  s
}

# разбор списка чисел из текстового поля
parse_nums <- function(s, min_n = 2) {
  parts <- strsplit(trimws(s), "[,;[:space:]]+")[[1]]
  parts <- parts[nzchar(parts)]
  x <- suppressWarnings(as.numeric(parts))
  if (length(x) == 0 || any(is.na(x)))
    stop("Проверьте числа: дробную часть отделяйте точкой, а сами числа — запятой или пробелом.")
  if (length(x) < min_n) stop(paste0("Нужно не меньше ", min_n, " чисел."))
  x
}

# проверка функции, введённой пользователем
check_expr <- function(s) {
  e <- tryCatch(str2lang(s), error = function(err)
    stop("Не удалось разобрать функцию. Пример записи: x^2*sin(x) (умножение — *, степень — ^, ln — log())."))
  extra <- setdiff(all.vars(e), c("x", "pi"))
  if (length(extra)) stop(paste0("Используйте только переменную x (найдено: ", paste(extra, collapse = ", "), ")."))
  paste(deparse(e, width.cutoff = 500L), collapse = "")
}
dp <- function(e) paste(deparse(e, width.cutoff = 500L), collapse = "")
code_html <- function(s) as.character(tags$code(s))

# таблица результатов
res_table <- function(...) {
  items <- list(...)
  tags$table(class = "res-table",
    lapply(names(items), function(n)
      tags$tr(tags$td(n), tags$td(HTML(as.character(items[[n]]))))))
}
verdict <- function(txt, good = TRUE)
  div(class = paste("verdict", if (good) "good" else "bad"), HTML(txt))

# поля ввода
ni <- function(id, label, value, step = NA) numericInput(id, label, value, step = step, width = "100%")
ti <- function(id, label, value) textInput(id, label, value, width = "100%")
ta <- function(id, label, value) textAreaInput(id, label, value, width = "100%", rows = 2)

# цвета графиков
COL1 <- "#13315c"; COL2 <- "#e0a800"; COL3 <- "#d1495b"; COLG <- "#e9edf3"
nice_par <- function() par(mar = c(4.2, 4.5, 2.5, 1), bg = "white", las = 1, cex.main = 1.05)

# конструктор задачи
task <- function(id, title, text, inputs, code, report, plot = NULL)
  list(id = id, title = title, text = text, inputs = inputs,
       code = code, report = report, plot = plot)

# ---------------------------------------------------------------------
#  БЛОК 1. ФИНАНСОВАЯ МАТЕМАТИКА
# ---------------------------------------------------------------------
m_choices <- c("Раз в год" = 1, "Раз в полгода" = 2, "Ежеквартально" = 4,
               "Ежемесячно" = 12, "Ежедневно" = 365)

fin_tasks <- list(

  task("f1", "Простые проценты",
    "Вклад <b>P</b> рублей размещён под простые проценты по ставке <b>r</b>% годовых на <b>t</b> лет.
     Найдите наращенную сумму и начисленные проценты.
     <div class='formula'>S = P · (1 + r · t)</div>",
    tagList(ni("f1_P", "Сумма вклада P, ₽", 100000), ni("f1_r", "Ставка r, % годовых", 12),
            ni("f1_t", "Срок t, лет", 3)),
    function(v) tpl(
"P <- {P}         # начальная сумма, руб.
r <- {r} / 100   # годовая ставка
t <- {t}         # срок, лет
S <- P * (1 + r * t)   # наращенная сумма
I <- S - P             # начисленные проценты",
      P = n2s(v("P")), r = n2s(v("r")), t = n2s(v("t"))),
    function(e) res_table("Наращенная сумма S" = rub(e$S), "Начисленные проценты I" = rub(e$I))),

  task("f2", "Сложные проценты",
    "На сумму <b>P</b> начисляются сложные проценты по ставке <b>r</b>% годовых <b>m</b> раз в год в течение <b>t</b> лет.
     Найдите наращенную сумму и сравните её с простыми процентами.
     <div class='formula'>S = P · (1 + r/m)<sup>m·t</sup></div>",
    tagList(ni("f2_P", "Сумма P, ₽", 100000), ni("f2_r", "Ставка r, % годовых", 10),
            ni("f2_t", "Срок t, лет", 5),
            selectInput("f2_m", "Капитализация", m_choices, selected = 12, width = "100%")),
    function(v) tpl(
"P <- {P}
r <- {r} / 100
t <- {t}
m <- {m}                        # число начислений в год
S <- P * (1 + r / m)^(m * t)    # сложные проценты
I <- S - P
S_simple <- P * (1 + r * t)     # для сравнения: простые проценты",
      P = n2s(v("P")), r = n2s(v("r")), t = n2s(v("t")), m = v("m")),
    function(e) res_table("Наращенная сумма S" = rub(e$S), "Начисленные проценты" = rub(e$I),
                          "По простым процентам" = rub(e$S_simple),
                          "Выгода сложных процентов" = rub(e$S - e$S_simple)),
    function(e) {
      nice_par(); tt <- seq(0, e$t, length.out = 200)
      yc <- e$P * (1 + e$r / e$m)^(e$m * tt); ys <- e$P * (1 + e$r * tt)
      plot(tt, yc, type = "l", lwd = 3, col = COL1, xlab = "Годы", ylab = "",
           main = "Рост вклада", ylim = range(yc, ys))
      lines(tt, ys, lwd = 3, col = COL2, lty = 2); grid(col = COLG)
      legend("topleft", c("Сложные", "Простые"), col = c(COL1, COL2), lwd = 3, lty = 1:2, bty = "n")
    }),

  task("f3", "Непрерывное начисление процентов",
    "Сумма <b>P</b> вложена под непрерывно начисляемые проценты с силой роста <b>r</b>% на <b>t</b> лет.
     Найдите наращенную сумму и сравните с ежегодной капитализацией.
     <div class='formula'>S = P · e<sup>r·t</sup></div>",
    tagList(ni("f3_P", "Сумма P, ₽", 50000), ni("f3_r", "Ставка r, %", 8), ni("f3_t", "Срок t, лет", 10)),
    function(v) tpl(
"P <- {P}
r <- {r} / 100
t <- {t}
S_cont <- P * exp(r * t)      # непрерывное начисление
S_year <- P * (1 + r)^t       # ежегодная капитализация",
      P = n2s(v("P")), r = n2s(v("r")), t = n2s(v("t"))),
    function(e) res_table("Непрерывное начисление" = rub(e$S_cont),
                          "Ежегодная капитализация" = rub(e$S_year),
                          "Разница" = rub(e$S_cont - e$S_year))),

  task("f4", "Дисконтирование (приведённая стоимость)",
    "Через <b>t</b> лет нужно получить сумму <b>S</b>. Ставка дисконтирования — <b>r</b>% годовых (сложные проценты).
     Какую сумму нужно вложить сегодня?
     <div class='formula'>PV = S / (1 + r)<sup>t</sup></div>",
    tagList(ni("f4_S", "Будущая сумма S, ₽", 1000000), ni("f4_r", "Ставка r, %", 9), ni("f4_t", "Срок t, лет", 4)),
    function(v) tpl(
"S <- {S}
r <- {r} / 100
t <- {t}
v <- 1 / (1 + r)^t    # дисконтный множитель
PV <- S * v           # приведённая (современная) стоимость
D <- S - PV           # дисконт",
      S = n2s(v("S")), r = n2s(v("r")), t = n2s(v("t"))),
    function(e) res_table("Нужно вложить сегодня PV" = rub(e$PV), "Дисконт D" = rub(e$D),
                          "Дисконтный множитель" = fm(e$v, 4))),

  task("f5", "Эффективная годовая ставка",
    "Банк объявил номинальную ставку <b>r</b>% годовых с начислением <b>m</b> раз в год.
     Найдите эффективную (реальную годовую) ставку.
     <div class='formula'>r<sub>эф</sub> = (1 + r/m)<sup>m</sup> − 1</div>",
    tagList(ni("f5_r", "Номинальная ставка r, %", 12),
            selectInput("f5_m", "Начисление", m_choices, selected = 12, width = "100%")),
    function(v) tpl(
"r <- {r} / 100
m <- {m}
r_eff <- (1 + r / m)^m - 1                 # эффективная ставка
m_all <- c(1, 2, 4, 12, 365)
r_all <- (1 + r / m_all)^m_all - 1         # для разных m
r_cont <- exp(r) - 1                       # непрерывное начисление",
      r = n2s(v("r")), m = v("m")),
    function(e) tagList(
      res_table("Эффективная ставка" = pct(e$r_eff, 3),
                "Раз в год" = pct(e$r_all[1], 3), "Раз в полгода" = pct(e$r_all[2], 3),
                "Ежеквартально" = pct(e$r_all[3], 3), "Ежемесячно" = pct(e$r_all[4], 3),
                "Ежедневно" = pct(e$r_all[5], 3), "Непрерывно" = pct(e$r_cont, 3))),
    function(e) {
      nice_par(); vals <- c(e$r_all, e$r_cont) * 100
      b <- barplot(vals, names.arg = c("1", "2", "4", "12", "365", "∞"), col = COL1, border = NA,
                   ylim = c(0, max(vals) * 1.15), main = "Эффективная ставка, %", xlab = "Начислений в год")
      text(b, vals, fm(vals, 2), pos = 3, cex = .85)
    }),

  task("f6", "Аннуитетный кредит",
    "Кредит <b>P</b> рублей выдан на <b>n</b> месяцев под <b>r</b>% годовых, погашается равными ежемесячными платежами.
     Найдите платёж, общую сумму выплат и переплату.
     <div class='formula'>A = P · i / (1 − (1 + i)<sup>−n</sup>),&nbsp; i = r/12</div>",
    tagList(ni("f6_P", "Сумма кредита P, ₽", 1500000), ni("f6_r", "Ставка r, % годовых", 16),
            ni("f6_n", "Срок n, месяцев", 36)),
    function(v) tpl(
"P <- {P}
r <- {r} / 100
n <- {n}
i <- r / 12                                    # месячная ставка
A <- if (i == 0) P / n else P * i / (1 - (1 + i)^(-n))   # ежемесячный платёж
total <- A * n                                 # всего выплат
over <- total - P                              # переплата
k <- 0:n
balance <- if (i == 0) P - A * k else P * (1 + i)^k - A * ((1 + i)^k - 1) / i   # остаток долга",
      P = n2s(v("P")), r = n2s(v("r")), n = n2s(round(v("n")))),
    function(e) res_table("Ежемесячный платёж A" = rub(e$A), "Всего выплат" = rub(e$total),
                          "Переплата" = rub(e$over)),
    function(e) {
      nice_par()
      plot(e$k, pmax(e$balance, 0), type = "h", lwd = 4, col = COL1, xlab = "Месяц", ylab = "",
           main = "Остаток долга по месяцам")
      grid(col = COLG)
    }),

  task("f7", "Накопление ежемесячными взносами",
    "Каждый месяц на счёт вносится <b>A</b> рублей под <b>r</b>% годовых с ежемесячной капитализацией.
     Сколько накопится за <b>t</b> лет?
     <div class='formula'>FV = A · ((1 + i)<sup>n</sup> − 1) / i,&nbsp; i = r/12,&nbsp; n = 12t</div>",
    tagList(ni("f7_A", "Взнос A, ₽/мес", 10000), ni("f7_r", "Ставка r, %", 10), ni("f7_t", "Срок t, лет", 3)),
    function(v) tpl(
"A <- {A}
r <- {r} / 100
t <- {t}
i <- r / 12
n <- round(12 * t)
FV <- if (i == 0) A * n else A * ((1 + i)^n - 1) / i   # накопленная сумма
paid <- A * n                                          # внесено своих денег
interest <- FV - paid                                  # заработано процентов
k <- 1:n
fv_k <- if (i == 0) A * k else A * ((1 + i)^k - 1) / i",
      A = n2s(v("A")), r = n2s(v("r")), t = n2s(v("t"))),
    function(e) res_table("Накоплено FV" = rub(e$FV), "Внесено своих" = rub(e$paid),
                          "Заработано процентов" = rub(e$interest)),
    function(e) {
      nice_par()
      plot(e$k, e$fv_k, type = "l", lwd = 3, col = COL1, xlab = "Месяц", ylab = "", main = "Рост накоплений")
      lines(e$k, e$A * e$k, lwd = 3, col = COL2, lty = 2); grid(col = COLG)
      legend("topleft", c("С процентами", "Только взносы"), col = c(COL1, COL2), lwd = 3, lty = 1:2, bty = "n")
    }),

  task("f8", "Чистая приведённая стоимость (NPV)",
    "Проект требует инвестиций <b>I₀</b> и приносит денежные потоки <b>CF₁ … CFₙ</b> в конце каждого года.
     Ставка дисконтирования <b>r</b>%. Найдите NPV и индекс доходности, оцените проект.
     <div class='formula'>NPV = Σ CF<sub>k</sub> / (1 + r)<sup>k</sup> − I₀</div>",
    tagList(ni("f8_I0", "Инвестиции I₀, ₽", 500000), ni("f8_r", "Ставка r, %", 12),
            ti("f8_cf", "Потоки CF по годам (через запятую)", "150000, 200000, 250000, 200000")),
    function(v) tpl(
"I0 <- {I0}
CF <- {CF}
r <- {r} / 100
k <- seq_along(CF)
PV <- CF / (1 + r)^k        # дисконтированные потоки
NPV <- sum(PV) - I0         # чистая приведённая стоимость
PI <- sum(PV) / I0          # индекс доходности",
      I0 = n2s(v("I0")), CF = vec2s(parse_nums(v("cf"), 1)), r = n2s(v("r"))),
    function(e) tagList(
      res_table("NPV" = rub(e$NPV), "Индекс доходности PI" = fm(e$PI, 3),
                "Сумма дисконтированных потоков" = rub(sum(e$PV))),
      if (e$NPV > 0) verdict("NPV &gt; 0 — проект <b>выгоден</b>, его стоит принять.")
      else verdict("NPV ≤ 0 — проект <b>не окупается</b> при данной ставке.", FALSE)),
    function(e) {
      nice_par(); cum <- c(-e$I0, cumsum(e$PV) - e$I0)
      b <- barplot(cum, names.arg = 0:length(e$PV), col = ifelse(cum >= 0, COL1, COL3), border = NA,
                   main = "Накопленный дисконтированный поток", xlab = "Год")
      abline(h = 0, col = "grey40")
    }),

  task("f9", "Внутренняя норма доходности (IRR)",
    "Для проекта с инвестициями <b>I₀</b> и потоками <b>CF₁ … CFₙ</b> найдите ставку, при которой NPV = 0.
     Решаем уравнение численно функцией <code>uniroot()</code>.
     <div class='formula'>Σ CF<sub>k</sub> / (1 + IRR)<sup>k</sup> − I₀ = 0</div>",
    tagList(ni("f9_I0", "Инвестиции I₀, ₽", 500000),
            ti("f9_cf", "Потоки CF по годам (через запятую)", "150000, 200000, 250000, 200000")),
    function(v) tpl(
"I0 <- {I0}
CF <- {CF}
npv <- function(r) sum(CF / (1 + r)^seq_along(CF)) - I0
if (npv(-0.99) * npv(10) > 0) stop(\"NPV не меняет знак — IRR для этих потоков не существует\")
IRR <- uniroot(npv, c(-0.99, 10), tol = 1e-10)$root",
      I0 = n2s(v("I0")), CF = vec2s(parse_nums(v("cf"), 1))),
    function(e) res_table("Внутренняя норма доходности IRR" = pct(e$IRR, 2),
                          "Проверка: NPV(IRR)" = fm(e$npv(e$IRR), 6)),
    function(e) {
      nice_par(); rr <- seq(max(-0.5, e$IRR - 0.2), max(0.3, e$IRR * 2), length.out = 300)
      y <- sapply(rr, e$npv)
      plot(rr * 100, y, type = "l", lwd = 3, col = COL1, xlab = "Ставка, %", ylab = "", main = "NPV в зависимости от ставки")
      abline(h = 0, col = "grey40"); abline(v = e$IRR * 100, col = COL2, lwd = 2, lty = 2); grid(col = COLG)
      points(e$IRR * 100, 0, pch = 19, col = COL3, cex = 1.4)
    }),

  task("f10", "Срок увеличения капитала в K раз",
    "За сколько лет капитал вырастет в <b>K</b> раз при ставке <b>r</b>% годовых и капитализации <b>m</b> раз в год?
     Для K = 2 сравним с «правилом 72».
     <div class='formula'>t = ln K / (m · ln(1 + r/m))</div>",
    tagList(ni("f10_K", "Во сколько раз K", 2), ni("f10_r", "Ставка r, %", 10),
            selectInput("f10_m", "Капитализация", m_choices, selected = 1, width = "100%")),
    function(v) tpl(
"K <- {K}
r <- {r} / 100
m <- {m}
t <- log(K) / (m * log(1 + r / m))   # точный срок, лет
t72 <- 72 / (r * 100)                # правило 72 (для удвоения)",
      K = n2s(v("K")), r = n2s(v("r")), m = v("m")),
    function(e) {
      rows <- list("Точный срок t" = paste(fm(e$t, 2), "лет"))
      if (abs(e$K - 2) < 1e-9) rows[["По правилу 72"]] <- paste(fm(e$t72, 2), "лет")
      do.call(res_table, rows)
    })
)

# ---------------------------------------------------------------------
#  БЛОК 2. НАХОЖДЕНИЕ ПРОИЗВОДНОЙ
# ---------------------------------------------------------------------
ev <- function(ex, xs) {
  y <- suppressWarnings(eval(ex, list(x = xs)))
  if (length(y) == 1) y <- rep(y, length(xs))
  y[!is.finite(y)] <- NA
  y
}
smart_ylim <- function(...) {
  y <- unlist(list(...)); y <- y[is.finite(y)]
  if (!length(y)) return(c(-1, 1))
  full <- range(y); rob <- quantile(y, c(.03, .97))
  if (diff(full) > 8 * diff(rob) && diff(rob) > 0) rob + c(-1, 1) * diff(rob) * .3 else full
}
deriv_plot <- function(e, extra = NULL, from = e$x - 4, to = e$x + 4) {
  nice_par(); xs <- seq(from, to, length.out = 600)
  y1 <- ev(e$f, xs); y2 <- ev(e$df, xs)
  ys <- list(y1, y2); if (!is.null(extra)) ys <- c(ys, list(ev(extra, xs)))
  plot(xs, y1, type = "n", ylim = do.call(smart_ylim, ys), xlab = "x", ylab = "", main = "Графики функции и производной")
  grid(col = COLG); abline(h = 0, v = 0, col = "grey60")
  lines(xs, y1, lwd = 3, col = COL1); lines(xs, y2, lwd = 3, col = COL2, lty = 2)
  leg <- c("f(x)", "f′(x)"); cols <- c(COL1, COL2); lt <- 1:2
  if (!is.null(extra)) { lines(xs, ys[[3]], lwd = 2, col = COL3, lty = 3); leg <- c(leg, "f″(x)"); cols <- c(cols, COL3); lt <- c(lt, 3) }
  points(e$x, e$f_x0, pch = 19, col = COL3, cex = 1.3)
  legend("topleft", leg, col = cols, lwd = 3, lty = lt, bty = "n")
}

d_code <- function(v, extra = "") tpl(
"f <- quote({f})      # исходная функция
df <- D(f, \"x\")      # производная по x (встроенная функция R)
x <- {x0}            # точка x0
f_x0 <- eval(f)      # значение функции в точке
df_x0 <- eval(df)    # значение производной в точке{extra}",
  f = check_expr(v("f")), x0 = n2s(v("x0")), extra = extra)

d_report <- function(e) res_table(
  "f(x)" = code_html(dp(e$f)), "f′(x)" = code_html(dp(e$df)),
  "f(x₀)" = fm(e$f_x0, 4), "f′(x₀)" = fm(e$df_x0, 4))

d_inputs <- function(id, f, x0) tagList(ti(paste0(id, "_f"), "Функция f(x)", f), ni(paste0(id, "_x0"), "Точка x₀", x0))

d_simple <- function(id, title, text, f, x0) task(id, title, text, d_inputs(id, f, x0),
  function(v) d_code(v), d_report, function(e) deriv_plot(e))

hint <- "<div class='hint'>Запись: умножение <code>*</code>, степень <code>^</code>, <code>sqrt()</code>, <code>exp()</code>, ln — <code>log()</code>, <code>sin()</code>, <code>cos()</code>, <code>tan()</code>.</div>"

der_tasks <- list(
  d_simple("d1", "Степенная функция",
    paste0("Найдите производную степенной функции и её значение в точке x₀.
     <div class='formula'>(x<sup>n</sup>)′ = n · x<sup>n−1</sup></div>", hint), "x^5", 2),
  d_simple("d2", "Многочлен",
    paste0("Найдите производную многочлена: производная суммы равна сумме производных, константа выносится за знак производной.
     <div class='formula'>(a·u + b·v)′ = a·u′ + b·v′</div>", hint), "3*x^4 - 2*x^3 + 5*x - 7", 1),
  d_simple("d3", "Производная произведения",
    paste0("Найдите производную произведения функций.
     <div class='formula'>(u · v)′ = u′ · v + u · v′</div>", hint), "x^2 * sin(x)", 1.5),
  d_simple("d4", "Производная частного",
    paste0("Найдите производную дроби.
     <div class='formula'>(u / v)′ = (u′ · v − u · v′) / v<sup>2</sup></div>", hint), "(x^2 + 1)/(x - 3)", 1),
  d_simple("d5", "Сложная функция",
    paste0("Найдите производную сложной функции по цепному правилу.
     <div class='formula'>f(g(x))′ = f′(g(x)) · g′(x)</div>", hint), "sin(x^2 + 1)", 1),
  d_simple("d6", "Экспонента и тригонометрия",
    paste0("Найдите производную функции с экспонентой.
     <div class='formula'>(e<sup>kx</sup>)′ = k · e<sup>kx</sup>,&nbsp; (cos x)′ = −sin x</div>", hint), "exp(2*x) * cos(x)", 0),
  d_simple("d7", "Логарифмическая функция",
    paste0("Найдите производную логарифма сложного аргумента.
     <div class='formula'>(ln u)′ = u′ / u</div>", hint), "log(x^2 + 4)", 2),

  task("d8", "Вторая производная и выпуклость",
    paste0("Найдите первую и вторую производные и определите направление выпуклости графика в точке x₀.
     <div class='formula'>f″(x) = (f′(x))′</div>", hint),
    d_inputs("d8", "x^4 - 3*x^2 + 2", 1),
    function(v) d_code(v, "
d2f <- D(df, \"x\")    # вторая производная
d2f_x0 <- eval(d2f)"),
    function(e) tagList(d_report(e),
      res_table("f″(x)" = code_html(dp(e$d2f)), "f″(x₀)" = fm(e$d2f_x0, 4)),
      if (e$d2f_x0 > 0) verdict("f″(x₀) &gt; 0 — в этой точке график <b>выпуклый вниз</b> (∪).")
      else if (e$d2f_x0 < 0) verdict("f″(x₀) &lt; 0 — в этой точке график <b>выпуклый вверх</b> (∩).", FALSE)
      else verdict("f″(x₀) = 0 — возможная <b>точка перегиба</b>.")),
    function(e) deriv_plot(e, extra = e$d2f)),

  task("d9", "Уравнение касательной",
    paste0("Составьте уравнение касательной к графику функции в точке x₀.
     <div class='formula'>y = f(x₀) + f′(x₀) · (x − x₀)</div>", hint),
    d_inputs("d9", "x^3 - 2*x", 2),
    function(v) d_code(v, "
k <- df_x0             # угловой коэффициент касательной
b <- f_x0 - k * x      # свободный член
# уравнение касательной: y = k*x + b"),
    function(e) tagList(d_report(e),
      verdict(paste0("Уравнение касательной: <b>y = ", fm(e$k, 4), "·x ",
                     if (e$b >= 0) "+ " else "− ", fm(abs(e$b), 4), "</b>"))),
    function(e) {
      deriv_plot(e)
      abline(a = e$b, b = e$k, col = COL3, lwd = 2)
    }),

  task("d10", "Экстремумы функции на отрезке",
    paste0("Найдите критические точки (f′(x) = 0) на отрезке [a; b], определите их тип по знаку f″,
     а также наибольшее и наименьшее значения функции на отрезке.", hint),
    tagList(ti("d10_f", "Функция f(x)", "x^3 - 6*x^2 + 9*x + 1"), ni("d10_a", "Начало отрезка a", 0),
            ni("d10_b", "Конец отрезка b", 5)),
    function(v) {
      if (v("a") >= v("b")) stop("Должно быть a < b.")
      tpl(
"f <- quote({f})
df <- D(f, \"x\")
d2f <- D(df, \"x\")
a <- {a}; b <- {b}
fp <- function(x) eval(df, list(x = x))
xs <- seq(a, b, length.out = 2001)
ys <- sapply(xs, fp)
idx <- which(diff(sign(ys)) != 0)              # смена знака f'
crit <- if (length(idx)) unique(round(sapply(idx, function(i)
          uniroot(fp, c(xs[i], xs[i + 1]))$root), 6)) else numeric(0)
type <- vapply(crit, function(c0) {
  s <- eval(d2f, list(x = c0))
  if (s > 0) \"минимум\" else if (s < 0) \"максимум\" else \"нужна доп. проверка\"
}, character(1))
pts <- c(a, crit, b)                           # точки-кандидаты
vals <- sapply(pts, function(p) eval(f, list(x = p)))
x_max <- pts[which.max(vals)]; f_max <- max(vals)
x_min <- pts[which.min(vals)]; f_min <- min(vals)",
      f = check_expr(v("f")), a = n2s(v("a")), b = n2s(v("b")))
    },
    function(e) {
      cr <- if (length(e$crit)) paste(sapply(seq_along(e$crit), function(i)
        paste0("x = ", fm(e$crit[i], 4), " — ", e$type[i], " (f = ", fm(eval(e$f, list(x = e$crit[i])), 4), ")")),
        collapse = "<br>") else "нет на отрезке"
      res_table("f′(x)" = code_html(dp(e$df)), "Критические точки" = cr,
                "Наибольшее значение" = paste0(fm(e$f_max, 4), " при x = ", fm(e$x_max, 4)),
                "Наименьшее значение" = paste0(fm(e$f_min, 4), " при x = ", fm(e$x_min, 4)))
    },
    function(e) {
      nice_par(); xs <- seq(e$a, e$b, length.out = 600); y <- ev(e$f, xs)
      plot(xs, y, type = "l", lwd = 3, col = COL1, xlab = "x", ylab = "", main = "Функция на отрезке [a; b]")
      grid(col = COLG)
      if (length(e$crit)) points(e$crit, sapply(e$crit, function(p) eval(e$f, list(x = p))), pch = 19, col = COL2, cex = 1.5)
      points(c(e$x_max, e$x_min), c(e$f_max, e$f_min), pch = 1, col = COL3, cex = 2.4, lwd = 2)
    })
)

# ---------------------------------------------------------------------
#  БЛОК 3. СТАТИСТИКА
# ---------------------------------------------------------------------
DATA1 <- "12, 15, 9, 20, 15, 18, 22, 15, 11, 17, 25, 14, 16, 19, 13"
alpha_choices <- c("0.10" = 0.10, "0.05" = 0.05, "0.01" = 0.01)
data_hint <- "<div class='hint'>Числа вводите через запятую или пробел, дробную часть — через точку.</div>"

stat_tasks <- list(

  task("s1", "Описательные статистики",
    paste0("По выборке найдите объём, среднее, медиану, моду, минимум, максимум и размах.", data_hint),
    ta("s1_x", "Выборка", DATA1),
    function(v) tpl(
"x <- {x}
n <- length(x)
m <- mean(x)                 # среднее арифметическое
med <- median(x)             # медиана
tab <- table(x)
moda <- as.numeric(names(tab)[tab == max(tab)])   # мода
rng <- range(x)              # минимум и максимум",
      x = vec2s(parse_nums(v("x")))),
    function(e) res_table("Объём n" = e$n, "Среднее" = fm(e$m, 3), "Медиана" = fm(e$med, 3),
      "Мода" = if (max(e$tab) == 1) "нет (все значения встречаются по разу)" else paste(e$moda, collapse = ", "),
      "Минимум / максимум" = paste(e$rng[1], "/", e$rng[2]), "Размах" = e$rng[2] - e$rng[1])),

  task("s2", "Дисперсия и стандартное отклонение",
    paste0("Найдите выборочную и генеральную дисперсии, стандартное отклонение и коэффициент вариации.
     <div class='formula'>s<sup>2</sup> = Σ(x<sub>i</sub> − x̄)<sup>2</sup> / (n − 1),&nbsp; V = s / x̄ · 100%</div>", data_hint),
    ta("s2_x", "Выборка", DATA1),
    function(v) tpl(
"x <- {x}
n <- length(x)
v_s <- var(x)              # выборочная (исправленная) дисперсия
s <- sd(x)                 # стандартное отклонение
v_p <- v_s * (n - 1) / n   # генеральная (смещённая) дисперсия
cv <- s / mean(x) * 100    # коэффициент вариации, %",
      x = vec2s(parse_nums(v("x")))),
    function(e) tagList(
      res_table("Выборочная дисперсия s²" = fm(e$v_s, 4), "Генеральная дисперсия σ²" = fm(e$v_p, 4),
                "Стандартное отклонение s" = fm(e$s, 4), "Коэффициент вариации V" = paste0(fm(e$cv, 2), "%")),
      if (e$cv <= 33) verdict("V ≤ 33% — совокупность <b>однородная</b>.")
      else verdict("V &gt; 33% — совокупность <b>неоднородная</b>.", FALSE))),

  task("s3", "Квартили и выбросы",
    paste0("Найдите квартили, межквартильный размах IQR и выбросы по правилу 1,5·IQR. Постройте «ящик с усами».", data_hint),
    ta("s3_x", "Выборка", "12, 15, 9, 20, 15, 18, 22, 15, 11, 17, 25, 14, 16, 19, 45"),
    function(v) tpl(
"x <- {x}
q <- quantile(x)                 # квартили
iqr <- IQR(x)                    # межквартильный размах
low <- q[2] - 1.5 * iqr          # нижняя граница
high <- q[4] + 1.5 * iqr         # верхняя граница
out <- x[x < low | x > high]     # выбросы",
      x = vec2s(parse_nums(v("x"), 4))),
    function(e) res_table("Q1 / Медиана / Q3" = paste(fm(e$q[2], 2), "/", fm(e$q[3], 2), "/", fm(e$q[4], 2)),
      "IQR" = fm(e$iqr, 2), "Границы нормы" = paste0("[", fm(e$low, 2), "; ", fm(e$high, 2), "]"),
      "Выбросы" = if (length(e$out)) paste(e$out, collapse = ", ") else "нет"),
    function(e) {
      nice_par()
      boxplot(e$x, horizontal = TRUE, col = "#c9d6ea", border = COL1, lwd = 2, main = "Ящик с усами", xlab = "Значения")
      stripchart(e$x, add = TRUE, method = "jitter", pch = 19, col = adjustcolor(COL2, .8))
    }),

  task("s4", "Интервальный ряд и гистограмма",
    paste0("Разбейте выборку на интервалы (число интервалов — по формуле Стёрджеса), найдите частоты и относительные частоты, постройте гистограмму.
     <div class='formula'>k ≈ 1 + log<sub>2</sub> n</div>", data_hint),
    ta("s4_x", "Выборка", "52, 61, 47, 58, 66, 70, 55, 49, 63, 59, 72, 68, 54, 60, 57, 65, 51, 62, 69, 56, 64, 58, 61, 53, 67"),
    function(v) tpl(
"x <- {x}
k <- nclass.Sturges(x)                    # число интервалов
h <- hist(x, breaks = k, plot = FALSE)    # интервальный ряд
freq <- data.frame(
  interval = paste0(\"(\", head(h$breaks, -1), \"; \", tail(h$breaks, -1), \"]\"),
  n = h$counts,                           # частоты
  w = round(h$counts / length(x), 3))     # относительные частоты",
      x = vec2s(parse_nums(v("x"), 5))),
    function(e) tagList(
      p(class = "muted", paste("Формула Стёрджеса дала k =", e$k, "; R подобрал «круглые» границы интервалов.")),
      tags$table(class = "res-table freq",
        tags$tr(tags$th("Интервал"), tags$th("Частота"), tags$th("Отн. частота")),
        lapply(seq_len(nrow(e$freq)), function(i)
          tags$tr(tags$td(e$freq$interval[i]), tags$td(e$freq$n[i]), tags$td(e$freq$w[i]))))),
    function(e) {
      nice_par(); plot(e$h, col = "#c9d6ea", border = COL1, main = "Гистограмма", xlab = "Значения", ylab = "Частота")
    }),

  task("s5", "Доверительный интервал для среднего",
    paste0("Постройте доверительный интервал для генерального среднего с заданной надёжностью (распределение Стьюдента).
     <div class='formula'>x̄ ± t<sub>γ, n−1</sub> · s / √n</div>", data_hint),
    tagList(ta("s5_x", "Выборка", DATA1),
            selectInput("s5_g", "Надёжность γ", c("90%" = 0.90, "95%" = 0.95, "99%" = 0.99), selected = 0.95, width = "100%")),
    function(v) tpl(
"x <- {x}
gamma <- {g}
n <- length(x); m <- mean(x); s <- sd(x)
t_cr <- qt(1 - (1 - gamma) / 2, df = n - 1)   # критическое значение Стьюдента
delta <- t_cr * s / sqrt(n)                    # предельная ошибка
ci <- c(m - delta, m + delta)
ci_check <- t.test(x, conf.level = gamma)$conf.int   # проверка встроенной функцией",
      x = vec2s(parse_nums(v("x"), 3)), g = v("g")),
    function(e) res_table("Среднее x̄" = fm(e$m, 4), "t критическое" = fm(e$t_cr, 4),
      "Предельная ошибка Δ" = fm(e$delta, 4),
      "Доверительный интервал" = paste0("<b>(", fm(e$ci[1], 4), "; ", fm(e$ci[2], 4), ")</b>"),
      "Проверка t.test()" = paste0("(", fm(e$ci_check[1], 4), "; ", fm(e$ci_check[2], 4), ")"))),

  task("s6", "Проверка гипотезы о среднем (t-тест)",
    paste0("Проверьте гипотезу H₀: генеральное среднее равно μ₀ против H₁: μ ≠ μ₀ на уровне значимости α.", data_hint),
    tagList(ta("s6_x", "Выборка", DATA1), ni("s6_mu", "Гипотетическое среднее μ₀", 15),
            selectInput("s6_a", "Уровень значимости α", alpha_choices, selected = 0.05, width = "100%")),
    function(v) tpl(
"x <- {x}
mu0 <- {mu}
alpha <- {a}
tt <- t.test(x, mu = mu0)        # одновыборочный t-тест
t_stat <- unname(tt$statistic)
p <- tt$p.value
n <- length(x)",
      x = vec2s(parse_nums(v("x"), 3)), mu = n2s(v("mu")), a = v("a")),
    function(e) tagList(
      res_table("Среднее выборки" = fm(mean(e$x), 4), "t-статистика" = fm(e$t_stat, 4), "p-value" = fm(e$p, 4)),
      if (e$p < e$alpha) verdict(paste0("p = ", fm(e$p, 4), " &lt; α — гипотеза H₀ <b>отвергается</b>: среднее отличается от ", e$mu0, "."), FALSE)
      else verdict(paste0("p = ", fm(e$p, 4), " ≥ α — <b>нет оснований отвергнуть</b> H₀: среднее можно считать равным ", e$mu0, "."))),
    function(e) {
      nice_par(); df <- e$n - 1; lim <- max(4, abs(e$t_stat) + 1); xs <- seq(-lim, lim, length.out = 400)
      tc <- qt(1 - e$alpha / 2, df)
      plot(xs, dt(xs, df), type = "l", lwd = 3, col = COL1, xlab = "t", ylab = "", main = "Распределение Стьюдента и критическая область")
      for (side in c(-1, 1)) {
        xx <- if (side < 0) seq(-lim, -tc, length.out = 100) else seq(tc, lim, length.out = 100)
        polygon(c(xx, rev(xx)), c(dt(xx, df), rep(0, 100)), col = adjustcolor(COL3, .35), border = NA)
      }
      abline(v = e$t_stat, col = COL2, lwd = 3)
    }),

  task("s7", "Сравнение средних двух выборок",
    paste0("Сравните средние двух независимых выборок (t-тест Уэлча). H₀: средние равны.", data_hint),
    tagList(ta("s7_x", "Выборка 1", "78, 85, 69, 90, 74, 88, 81, 77"),
            ta("s7_y", "Выборка 2", "72, 70, 65, 80, 68, 75, 71, 69"),
            selectInput("s7_a", "Уровень значимости α", alpha_choices, selected = 0.05, width = "100%")),
    function(v) tpl(
"x <- {x}
y <- {y}
alpha <- {a}
tt <- t.test(x, y)               # t-тест Уэлча
p <- tt$p.value",
      x = vec2s(parse_nums(v("x"), 2)), y = vec2s(parse_nums(v("y"), 2)), a = v("a")),
    function(e) tagList(
      res_table("Среднее 1" = fm(mean(e$x), 3), "Среднее 2" = fm(mean(e$y), 3),
                "t-статистика" = fm(unname(e$tt$statistic), 4), "p-value" = fm(e$p, 4)),
      if (e$p < e$alpha) verdict("p &lt; α — средние <b>статистически различаются</b>.", FALSE)
      else verdict("p ≥ α — <b>значимых различий</b> между средними нет.")),
    function(e) {
      nice_par()
      boxplot(list("Выборка 1" = e$x, "Выборка 2" = e$y), col = c("#c9d6ea", "#f6e3a1"), border = COL1, lwd = 2,
              main = "Сравнение выборок")
      points(1:2, c(mean(e$x), mean(e$y)), pch = 18, cex = 2, col = COL3)
    }),

  task("s8", "Корреляция Пирсона",
    paste0("Найдите коэффициент корреляции между X и Y, проверьте его значимость и оцените силу связи по шкале Чеддока.", data_hint),
    tagList(ta("s8_x", "X (например, расходы на рекламу)", "10, 15, 20, 25, 30, 35, 40"),
            ta("s8_y", "Y (например, продажи)", "120, 150, 185, 200, 240, 260, 300")),
    function(v) tpl(
"x <- {x}
y <- {y}
if (length(x) != length(y)) stop(\"Ряды X и Y должны быть одной длины\")
r <- cor(x, y)               # коэффициент корреляции Пирсона
ct <- cor.test(x, y)         # проверка значимости
p <- ct$p.value",
      x = vec2s(parse_nums(v("x"), 3)), y = vec2s(parse_nums(v("y"), 3))),
    function(e) {
      a <- abs(e$r)
      s <- if (a < .1) "практически отсутствует" else if (a < .3) "слабая" else if (a < .5) "умеренная" else
           if (a < .7) "заметная" else if (a < .9) "высокая" else "весьма высокая"
      tagList(res_table("r" = fm(e$r, 4), "p-value" = fm(e$p, 5), "Сила связи" = s,
                        "Направление" = if (e$r >= 0) "прямая" else "обратная"),
        if (e$p < 0.05) verdict("p &lt; 0,05 — корреляция <b>значима</b>.") else verdict("p ≥ 0,05 — корреляция <b>незначима</b>.", FALSE))
    },
    function(e) {
      nice_par(); plot(e$x, e$y, pch = 19, col = COL1, cex = 1.5, xlab = "X", ylab = "Y", main = "Диаграмма рассеяния")
      grid(col = COLG)
    }),

  task("s9", "Парная линейная регрессия",
    paste0("Постройте уравнение регрессии y = a + b·x методом наименьших квадратов, найдите R² и сделайте прогноз для x₀.", data_hint),
    tagList(ta("s9_x", "X", "10, 15, 20, 25, 30, 35, 40"), ta("s9_y", "Y", "120, 150, 185, 200, 240, 260, 300"),
            ni("s9_x0", "Прогноз для x₀", 45)),
    function(v) tpl(
"x <- {x}
y <- {y}
if (length(x) != length(y)) stop(\"Ряды X и Y должны быть одной длины\")
model <- lm(y ~ x)                       # МНК
a <- unname(coef(model)[1])
b <- unname(coef(model)[2])
R2 <- summary(model)$r.squared           # коэффициент детерминации
x0 <- {x0}
y0 <- unname(predict(model, data.frame(x = x0)))   # прогноз",
      x = vec2s(parse_nums(v("x"), 3)), y = vec2s(parse_nums(v("y"), 3)), x0 = n2s(v("x0"))),
    function(e) tagList(
      verdict(paste0("Уравнение: <b>y = ", fm(e$a, 3), if (e$b >= 0) " + " else " − ", fm(abs(e$b), 3), "·x</b>")),
      res_table("a (свободный член)" = fm(e$a, 4), "b (наклон)" = fm(e$b, 4), "R²" = fm(e$R2, 4),
                "Прогноз y(x₀)" = fm(e$y0, 3))),
    function(e) {
      nice_par(); xr <- range(c(e$x, e$x0))
      plot(e$x, e$y, pch = 19, col = COL1, cex = 1.5, xlim = xr, ylim = range(c(e$y, e$y0)),
           xlab = "X", ylab = "Y", main = "Линия регрессии и прогноз")
      grid(col = COLG); abline(e$model, col = COL2, lwd = 3)
      points(e$x0, e$y0, pch = 17, col = COL3, cex = 1.8)
    }),

  task("s10", "Нормальное распределение",
    "Случайная величина X распределена нормально с параметрами μ и σ. Найдите вероятность P(a &lt; X &lt; b).
     <div class='formula'>P(a &lt; X &lt; b) = Φ((b − μ)/σ) − Φ((a − μ)/σ)</div>",
    tagList(ni("s10_mu", "Среднее μ", 100), ni("s10_s", "Стандартное отклонение σ", 15),
            ni("s10_a", "a", 85), ni("s10_b", "b", 115)),
    function(v) {
      if (v("s") <= 0) stop("σ должно быть больше нуля.")
      if (v("a") >= v("b")) stop("Должно быть a < b.")
      tpl(
"mu <- {mu}; sigma <- {s}
a <- {a}; b <- {b}
z_a <- (a - mu) / sigma; z_b <- (b - mu) / sigma     # стандартизация
P <- pnorm(b, mu, sigma) - pnorm(a, mu, sigma)       # искомая вероятность",
        mu = n2s(v("mu")), s = n2s(v("s")), a = n2s(v("a")), b = n2s(v("b")))
    },
    function(e) res_table("z(a) / z(b)" = paste(fm(e$z_a, 3), "/", fm(e$z_b, 3)),
                          "P(a < X < b)" = paste0("<b>", fm(e$P, 4), "</b> (", fm(e$P * 100, 2), "%)")),
    function(e) {
      nice_par(); xs <- seq(e$mu - 4 * e$sigma, e$mu + 4 * e$sigma, length.out = 400)
      plot(xs, dnorm(xs, e$mu, e$sigma), type = "l", lwd = 3, col = COL1, xlab = "x", ylab = "", main = "Плотность нормального распределения")
      xx <- seq(max(e$a, min(xs)), min(e$b, max(xs)), length.out = 200)
      polygon(c(xx, rev(xx)), c(dnorm(xx, e$mu, e$sigma), rep(0, length(xx))), col = adjustcolor(COL2, .5), border = NA)
    })
)

all_tasks <- c(fin_tasks, der_tasks, stat_tasks)

# ---------------------------------------------------------------------
#  Интерфейс
# ---------------------------------------------------------------------
task_ui <- function(t, num) {
  tabPanel(paste0(num, ". ", t$title), value = t$id,
    div(class = "task-card",
      div(class = "task-num", paste("Задача", num)),
      h3(t$title),
      div(class = "task-text", HTML(t$text)),
      div(class = "task-inputs", t$inputs),
      actionButton(paste0(t$id, "_go"), "Решить", class = "btn-solve"),
      uiOutput(paste0(t$id, "_res")),
      if (!is.null(t$plot)) div(class = "plot-box", plotOutput(paste0(t$id, "_plot"), height = "330px")),
      tags$details(class = "code-box", open = NA,
        tags$summary("Код на R, который выполнил сайт"),
        verbatimTextOutput(paste0(t$id, "_code")))
    ))
}

block_ui <- function(tasks, id, title, subtitle) {
  div(class = "block",
    div(class = "block-head", h2(title), p(subtitle)),
    do.call(navlistPanel, c(list(id = paste0(id, "_list"), widths = c(3, 9), well = FALSE),
                            lapply(seq_along(tasks), function(i) task_ui(tasks[[i]], i)))))
}

home_card <- function(id, emoji, title, text)
  div(class = "home-card",
    div(class = "home-emoji", emoji), h4(title), p(text),
    actionButton(paste0("go_", id), "Перейти к задачам →", class = "btn-go"))

css <- "
:root { --navy:#13315c; --navy2:#0b2545; --gold:#e0a800; --bg:#f4f6fa; }
body { background: var(--bg); }
.navbar { background: var(--navy2) !important; box-shadow: 0 2px 10px rgba(0,0,0,.15); }
.navbar .navbar-brand { font-weight: 800; letter-spacing: .3px; color: #fff !important; }
.navbar .nav-link { color: #cfd8e6 !important; font-weight: 600; }
.navbar .nav-link.active, .navbar .nav-link:hover { color: var(--gold) !important; }
.container-fluid > .tab-content { padding-top: 8px; }
.hero { display:flex; flex-wrap:wrap; align-items:center; gap:28px; background: linear-gradient(135deg, #13315c 0%, #1f4a85 60%, #2b5c9e 100%);
  color:#fff; border-radius: 22px; padding: 34px 38px; margin: 10px 0 26px; box-shadow: 0 10px 30px rgba(19,49,92,.25); }
.hero-text { flex: 1 1 340px; }
.hero h1 { font-weight: 800; font-size: 2.4rem; margin-bottom: 10px; }
.hero h1 span { color: var(--gold); }
.hero p { font-size: 1.08rem; color: #e3e9f3; max-width: 560px; }
.hero-badges span { display:inline-block; background: rgba(255,255,255,.12); border:1px solid rgba(255,255,255,.25);
  border-radius: 999px; padding: 4px 12px; margin: 6px 6px 0 0; font-size: .88rem; }
.hero-cat { flex: 0 0 auto; text-align:center; position: relative; }
.hero-cat img { width: 250px; max-width: 70vw; filter: drop-shadow(0 8px 14px rgba(0,0,0,.25)); animation: float 4s ease-in-out infinite; }
@keyframes float { 0%,100% { transform: translateY(0) } 50% { transform: translateY(-8px) } }
.speech { background:#fff; color: var(--navy2); border-radius: 16px; padding: 10px 14px; font-weight: 600; font-size: .95rem;
  max-width: 250px; margin: 0 auto 10px; position: relative; box-shadow: 0 4px 12px rgba(0,0,0,.15); }
.speech:after { content:''; position:absolute; bottom:-10px; left: 50%; border: 10px solid transparent; border-top-color:#fff; border-bottom:0; margin-left:-10px; }
.home-cards { display:grid; grid-template-columns: repeat(auto-fit, minmax(240px, 1fr)); gap: 18px; margin-bottom: 26px; }
.home-card { background:#fff; border-radius: 18px; padding: 22px; box-shadow: 0 4px 16px rgba(19,49,92,.08); border-top: 5px solid var(--gold);
  transition: transform .15s, box-shadow .15s; }
.home-card:hover { transform: translateY(-4px); box-shadow: 0 10px 24px rgba(19,49,92,.15); }
.home-emoji { font-size: 2.2rem; }
.home-card h4 { font-weight: 800; color: var(--navy); margin-top: 6px; }
.home-card p { color:#4a5568; min-height: 48px; }
.btn-go, .btn-solve { background: var(--navy) !important; color:#fff !important; border:none !important; border-radius: 10px !important;
  font-weight: 700 !important; padding: 8px 18px !important; }
.btn-go:hover, .btn-solve:hover { background: var(--gold) !important; color: var(--navy2) !important; }
.how { background:#fff; border-radius: 18px; padding: 20px 24px; box-shadow: 0 4px 16px rgba(19,49,92,.08); margin-bottom: 30px; }
.how h4 { font-weight: 800; color: var(--navy); }
.how ol { margin-bottom: 0; }
.block-head h2 { font-weight: 800; color: var(--navy); margin: 8px 0 2px; }
.block-head p { color: #5a6475; }
.nav-pills .nav-link, .nav-stacked > li > a { border-radius: 10px; color: var(--navy); font-weight: 600; padding: 9px 12px; margin-bottom: 4px; background:#fff; }
.nav-pills .nav-link.active { background: var(--navy) !important; color:#fff !important; }
.task-card { background:#fff; border-radius: 18px; padding: 24px 26px; box-shadow: 0 4px 16px rgba(19,49,92,.08); margin-bottom: 30px; }
.task-num { color: var(--gold); font-weight: 800; text-transform: uppercase; font-size: .8rem; letter-spacing: 1px; }
.task-card h3 { font-weight: 800; color: var(--navy); margin-top: 2px; }
.task-text { color:#2d3748; margin-bottom: 14px; }
.formula { background: #f0f4fb; border-left: 4px solid var(--navy); border-radius: 8px; padding: 10px 14px; margin: 10px 0;
  font-family: Georgia, 'Times New Roman', serif; font-size: 1.12rem; color: var(--navy2); }
.hint { font-size: .86rem; color: #6b7280; margin-top: 6px; }
.task-inputs { display:grid; grid-template-columns: repeat(auto-fill, minmax(210px, 1fr)); gap: 0 16px; }
.res-table { width: 100%; margin: 16px 0 6px; border-collapse: collapse; }
.res-table td, .res-table th { padding: 8px 10px; border-bottom: 1px solid #e6eaf0; vertical-align: top; }
.res-table th { background: #f0f4fb; color: var(--navy); }
.res-table td:first-child { color:#5a6475; width: 44%; }
.res-table td:last-child { font-weight: 700; color: var(--navy2); }
.freq td:first-child { width:auto; }
.verdict { border-radius: 10px; padding: 10px 14px; margin: 10px 0; }
.verdict.good { background: #e8f6ee; color: #1e6b3a; }
.verdict.bad { background: #fdecec; color: #8a1f2b; }
.error-box { background:#fdecec; color:#8a1f2b; border-radius: 10px; padding: 10px 14px; margin-top: 14px; font-weight: 600; }
.plot-box { margin-top: 12px; }
.code-box { margin-top: 14px; }
.code-box summary { cursor: pointer; color: var(--navy); font-weight: 700; }
.code-box pre { background: #0b2545; color: #e6edf7; border-radius: 10px; padding: 14px; font-size: .86rem; margin-top: 8px; white-space: pre-wrap; }
.muted { color:#6b7280; margin-top: 10px; }
.footer { text-align:center; color:#8a94a6; font-size: .88rem; padding: 10px 0 24px; }
"

ui <- navbarPage(
  title = "🐱 R Math Solver",
  id = "nav",
  windowTitle = "R Math Solver — решатель задач на R",
  theme = bs_theme(version = 5, primary = "#13315c", secondary = "#e0a800"),
  header = tags$head(tags$style(HTML(css))),
  footer = div(class = "footer", "R Math Solver · сделано на R + Shiny · работает в браузере через shinylive (webR)"),

  tabPanel("Главная", value = "home",
    div(class = "hero",
      div(class = "hero-text",
        h1(HTML("Решатель задач <span>на R</span>")),
        p("Сайт решает задачи по финансовой математике, нахождению производных и статистике.
           Введите свои данные — и R посчитает ответ, построит график и покажет код решения."),
        div(class = "hero-badges", span("3 блока"), span("30 задач"), span("Код на R"), span("Графики"))),
      div(class = "hero-cat",
        div(class = "speech", "Мяу! Я Финик, кот-маскот Финашки. Выбирай блок — порешаем!"),
        img(src = "cat.svg", alt = "Кот-маскот Финик"))),
    div(class = "home-cards",
      home_card("fin", "💰", "Финансовая математика",
                "Проценты, дисконтирование, кредиты, накопления, NPV и IRR."),
      home_card("der", "📈", "Нахождение производной",
                "Производные функций через D(), касательная, выпуклость, экстремумы."),
      home_card("stat", "📊", "Статистика",
                "Описательные статистики, интервалы, t-тесты, корреляция, регрессия.")),
    div(class = "how",
      h4("Как пользоваться"),
      tags$ol(
        tags$li("Выберите блок в меню сверху или карточкой выше."),
        tags$li("Слева выберите одну из 10 задач."),
        tags$li("Введите свои данные и нажмите «Решить»."),
        tags$li("Под ответом — график и код на R, который выполнил сайт.")))),

  tabPanel("Финансовая математика", value = "fin",
    block_ui(fin_tasks, "fin", "Финансовая математика", "10 задач: проценты, дисконтирование, кредиты и инвестиции")),
  tabPanel("Производные", value = "der",
    block_ui(der_tasks, "der", "Нахождение производной", "10 задач: правила дифференцирования, касательная, экстремумы")),
  tabPanel("Статистика", value = "stat",
    block_ui(stat_tasks, "stat", "Статистика", "10 задач: описательная статистика, гипотезы, корреляция и регрессия"))
)

# ---------------------------------------------------------------------
#  Сервер
# ---------------------------------------------------------------------
server <- function(input, output, session) {

  observeEvent(input$go_fin,  updateNavbarPage(session, "nav", selected = "fin"))
  observeEvent(input$go_der,  updateNavbarPage(session, "nav", selected = "der"))
  observeEvent(input$go_stat, updateNavbarPage(session, "nav", selected = "stat"))

  lapply(all_tasks, function(t) {
    res <- eventReactive(input[[paste0(t$id, "_go")]], {
      v <- function(nm) {
        val <- input[[paste0(t$id, "_", nm)]]
        if (is.null(val) || (is.numeric(val) && is.na(val)) || (is.character(val) && !nzchar(trimws(val))))
          stop("Заполните все поля задачи.")
        val
      }
      code <- ""
      tryCatch({
        code <- t$code(v)
        env <- new.env(parent = globalenv())
        eval(parse(text = code, keep.source = FALSE), envir = env)
        list(code = code, env = env, ui = t$report(env))
      }, error = function(err) list(error = conditionMessage(err), code = code))
    }, ignoreNULL = FALSE)

    output[[paste0(t$id, "_res")]] <- renderUI({
      r <- res()
      if (!is.null(r$error)) return(div(class = "error-box", paste("Ошибка:", r$error)))
      div(class = "result", r$ui)
    })
    output[[paste0(t$id, "_code")]] <- renderText({
      r <- res()
      if (nzchar(r$code)) r$code else "# код появится после ввода корректных данных"
    })
    if (!is.null(t$plot)) {
      output[[paste0(t$id, "_plot")]] <- renderPlot({
        r <- res()
        req(is.null(r$error))
        t$plot(r$env)
      }, res = 96)
    }
  })
}

shinyApp(ui, server)
