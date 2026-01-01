# Burkina Faso - Application Shiny Interactive

## 🌍 Application en ligne
**[Accéder à l'application](https://mesapplications.shinyapps.io/Burkina_Faso_Subdivisions_2025/)**

## Description
Application web interactive pour explorer les subdivisions administratives du Burkina Faso (réforme 2025).

## Fonctionnalités
- 🗺️ **Carte interactive** avec zoom et survol
- 📊 **Tableaux de données** filtrables et triables
- 📈 **Graphiques** de population et densité
- 🔍 **Navigation** par région, province et commune

## Données
- **17 régions** | **47 provinces** | **351 communes**
- **Population totale** : 20 505 155 habitants (RGPH 2019)
- **Superficie** : 274 220 km²

## Structure
```
├── app.R           # Interface utilisateur et serveur
├── global.R        # Chargement et préparation des données
├── data/           # Données (shapefiles + population)
└── www/styles.css  # Styles CSS personnalisés
```

## Déploiement
```r
# Installer rsconnect si nécessaire
install.packages("rsconnect")

# Configurer les identifiants shinyapps.io
rsconnect::setAccountInfo(name='VOTRE_NOM', token='TOKEN', secret='SECRET')

# Déployer
rsconnect::deployApp(".")
```

## Technologies
- R Shiny
- Leaflet (cartes interactives)
- Plotly (graphiques)
- sf (données géographiques)

## Auteur
AWOUTO K. Samson - Élève Ingénieur Statisticien Économiste, ENSAE Dakar

## 🛡️ Propriété Intellectuelle
Ce projet est protégé par la licence **CC BY-NC-SA 4.0**.
Toute utilisation commerciale est interdite sans autorisation explicite de l'auteur.

## 📜 Citation
> AWOUTO, K. S. (2026). *Application Shiny - Subdivisions Burkina Faso 2025*. ENSAE Dakar. https://github.com/Awoutokoffisamson/burkina_application_shiny

