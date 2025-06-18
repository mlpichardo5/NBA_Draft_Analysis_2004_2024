# NBA Draft Analysis (2004–2024)

## A Data-Driven Evaluation of Team Performance in the NBA Draft

Welcome to my data science project where I combined basketball knowledge with rigorous statistical modeling to evaluate **how well NBA teams have drafted from 2004 to 2024**.

This project highlights my ability to:
- Build and clean large datasets
- Create custom performance metrics
- Apply advanced statistical models 
- Generate compelling data visualizations
- Translate complex results into actionable insights

The full write-up is included in the docs folder

---

## Tools & Technologies

- **Language**: R (Tidyverse, mgcv, ggplot2)
- **Statistical Techniques**:  
  - Generalized Additive Models (GAM)  
  - Z-score normalization  
  - Custom weighting functions  
- **Data Visualization**:  
  - Draft performance quadrants  
  - Pick value curves  
  - Custom metric comparisons by team and class  

---

## Custom Metrics

All metrics are built on **Value per Season (VPS)** (to adjust for career-length discrepancies) which was created using **Value Over Replacement Player (VORP)**:

| Metric        | Description |
|---------------|-------------|
| **VPS**       | VORP per Season: averages VORP over career to remove career length bias |
| **VOADP**     | Value Over Average Drafted Player: evaluates players against their draft class's average VPS |
| **EPV / dEPV**| Expected Pick Value and deviation from it, modeled using GAM |
| **WdEPV**     | Weighted dEPV: rewards teams for exceeding expectations with late-round picks |

---

## Key Findings

- **Top performers**: Thunder, Spurs, Nuggets, Pelicans
- **Teams that underperformed**: Hornets, Kings, Wizards
- **Biggest value picks**: Jokic, Butler, Gobert, Brunson
- **Conclusion**: Successful draft teams tend to hit on one or more superstar lottery picks, then build around them with high-efficiency role players using later picks.

---

## Repo Structure
├── data/ # Cleaned datasets
├── scripts/ # R scripts for cleaning, modeling, and visualizing
├── figures/ # Output plots and graphics
├── docs/ # Final article
└── README.md


---

## What This Shows About Me

- Strong foundation in **applied statistics** and **predictive modeling**
- Comfort working with **noisy, real-world sports data**
- Ability to communicate technical insights clearly through **writing and visualization**
- Passion for **using data to challenge narratives and uncover truth**

---

## Contact

**Lucas Pichardo**  
* UCLA – B.S. in Statistics & Data Science  
* [LinkedIn](https://www.linkedin.com/in/lucaspichardo/); I’d love to connect.

