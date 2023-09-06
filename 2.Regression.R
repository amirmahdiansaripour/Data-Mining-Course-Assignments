cereals = read.csv("C:\\Users\\amirmahdi\\Desktop\\Miningdata\\datasets\\cereal.csv", header = TRUE);
cereals[1:5, ];

fibers = cereals$fiber;
ratings = cereals$rating;
sugars = cereals$sugars;

## Regression estimation Model
lm.out = lm(ratings~fibers)
plot(
  fibers,
  ratings,
  main = "Estimation of Rating Based on Fiber Content",
  xlab = "Fiber + Sugar content",
  ylab = "Rating"
)
abline(lm.out)
summary(lm.out)
# Rating[x] = 35.2566 + 3.4430 * Fiber[x]

# Confidence Interval
confidence_interval = predict(lm.out, data.frame(fibers = 3), interval = "confidence")
print(confidence_interval);

# Prediction Interval
predict(lm.out, data.frame(fibers = 3), interval = "prediction")


# Normality plot + Linearity assumption
par(mfrow = c(2, 2))
plot(lm.out)

mreg.out = lm(ratings~(sugars + fibers))
plot(
  (sugars + fibers),
  ratings,
  main = "Estimation of Rating Based on Sugar + Fiber",
  xlab = "Sugar + Fiber Content",
  ylab = "Rating"
)
abline(mreg.out)
summary(mreg.out)