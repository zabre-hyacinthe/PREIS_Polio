# PREIS_Polio_FV — Documentation technique et opérationnelle
**Version finale — Avril 2026**
**Africa CDC — Surveillance et Intelligence épidémiologique**

---

## 1. Vue d'ensemble

PREIS_Polio_FV est un système automatisé de surveillance épidémiologique hebdomadaire du **poliovirus en Afrique**. Il collecte les données de l'Initiative Mondiale pour l'Éradication de la Poliomyélite (GPEI), les distribue aux équipes des Centres de Coordination Régionaux (RCC) d'Africa CDC, et met à jour un dashboard de visualisation en ligne.

### Ce que le système fait chaque semaine

1. Détecte automatiquement la publication hebdomadaire GPEI
2. Extrait les données pays africains affectés
3. Envoie des emails personnalisés par RCC aux 8 destinataires
4. Met à jour le dashboard shinyapps.io

**Aucune intervention manuelle n'est nécessaire.**

---

## 2. Architecture du projet

```
D:/PREIS_Polio_FV/
│
├── R/                              Scripts R de production
│   ├── 00_load_polio_project.R     Chargement global
│   ├── 02_check_polio_update.R     Détection nouvelle issue GPEI
│   ├── 03_issue_registry.R         Registre last_issue
│   ├── 04_run_polio_pipeline_core.R  Core pipeline
│   ├── 05_prepare_polio_alert_input.R  Extraction + mapping RCC
│   ├── 09_send_polio_rcc_emails.R  Construction emails RCC
│   ├── 09b_send_polio_rcc_emails_conditional.R  Envoi conditionnel
│   ├── 60_email.R                  Moteur email (blastula/SMTP)
│   ├── 100_run_polio_pipeline_if_update.R  Orchestrateur conditionnel
│   └── 110_run_polio_production_pipeline.R  Point d'entrée principal
│
├── dashboard/                      Application Shiny
│   ├── app.R                       Dashboard interactif
│   ├── config/                     Fichiers de référence
│   │   ├── rcc_country_fv.csv      Mapping pays → RCC (49 pays)
│   │   ├── africa_gazetteer.csv    Gazetier Afrique
│   │   └── rcc_country_fv.csv     Coordonnées pays
│   └── data/
│       ├── dashboard/              Données opérationnelles
│       │   ├── polio_africa_email_input.csv  Données Polio (sortie pipeline)
│       │   ├── polio_alert_input.csv         Alertes
│       │   ├── polio_last_issue.csv          Métadonnées issue courante
│       │   └── alert_recipients.csv          Liste des destinataires
│       └── curated/
│           └── africa_countries_rcc.geojson  Carte Afrique RCC
│
├── config/                         Référentiels partagés
├── data/
│   ├── last_issue.txt              Registre de la dernière issue traitée
│   ├── processed/                  Données intermédiaires
│   └── archive/                    Backups automatiques
│
├── outputs/
│   ├── logs/
│   │   └── polio_pipeline.log      Log d'exécution du pipeline
│   └── email/                      Copies des emails envoyés
│
└── run_polio_pipeline.bat          Lanceur Windows
```

---

## 3. Pipeline de traitement

### Flux complet

```
[Tâche planifiée Windows — tous les jours 8h00]
                    ↓
     110_run_polio_production_pipeline.R
                    ↓
     00_load_polio_project.R  (chargement)
                    ↓
     02_check_polio_update.R  (scraping GPEI)
                    ↓
          Nouvelle issue ?
         /              \
        NON              OUI
         ↓                ↓
      Arrêt           05_prepare_polio_alert_input.R
    (2 secondes)           ↓
                    Extraction bullets
                    Mapping pays → RCC
                           ↓
                    09_send_polio_rcc_emails.R
                           ↓
                    Emails envoyés par RCC
                           ↓
                    last_issue.txt mis à jour
                           ↓
                    Dashboard redéployé (shinyapps.io)
                           ↓
                    Log : STATUT SUCCESS
```

### Logique de déduplication

Le système mémorise la dernière issue traitée dans `data/last_issue.txt`.
Format : `polio_this_week__DD_month_YYYY`

À chaque exécution, il compare l'issue GPEI courante avec la valeur enregistrée.
Si identique → arrêt immédiat. Si différente → pipeline complet.

---

## 4. Sources de données

### GPEI — Global Polio Eradication Initiative
- **URL** : https://polioeradication.org/about-polio/polio-this-week/
- **Fréquence** : hebdomadaire (généralement le jeudi)
- **Format** : page HTML — bullets pays

### Référentiel RCC Africa CDC
- **Fichier** : `config/rcc_country_fv.csv`
- **Contenu** : 49 pays africains avec ISO3 et RCC
- **RCC couverts** : Northern, Western, Central, Eastern, Southern

---

## 5. Destinataires et couverture RCC

| Email | RCC | Portée |
|---|---|---|
| zrhyacinthe@gmail.com | All | Toute l'Afrique |
| zrhyacinthe2@gmail.com | All | Toute l'Afrique |
| zraogohyacinthe@yahoo.fr | Northern | Afrique du Nord |
| raogoz@africacdc.org | Western | Afrique de l'Ouest |
| carefordbf@gmail.com | Central | Afrique Centrale |
| reassadbf@gmail.com | Eastern | Afrique de l'Est |
| zrhyacinthe2@gmail.com | Southern | Afrique Australe |
| zrhyacinthe2@gmail.com | All | Toute l'Afrique |

**Logique d'envoi** : chaque destinataire reçoit uniquement les alertes
correspondant à son RCC. Si aucune alerte pour un RCC donné → email ignoré
(pas de message vide envoyé).

---

## 6. Dashboard

- **URL** : https://[compte].shinyapps.io/dashboard
- **Mise à jour** : automatique après chaque nouvelle issue GPEI
- **Contenu** :
  - Carte Afrique interactive avec marqueurs RCC
  - Statistiques : cas, pays affectés, RCC représentés
  - Types de virus : WPV1, cVDPV1, cVDPV2
  - Sources de détection : Humaine, Environnementale, Les deux
  - Évolution temporelle
  - Tableau détaillé filtrable
  - Export CSV

---

## 7. Configuration SMTP

Les identifiants SMTP sont définis comme variables d'environnement R.
Ils ne sont jamais stockés dans les scripts.

```r
# À définir avant chaque session (ou dans .Renviron)
Sys.setenv(SMTP_USER  = "email@domaine.com")
Sys.setenv(SMTP_PASS  = "mot_de_passe")
Sys.setenv(SMTP_HOST  = "smtp.gmail.com")   # défaut
Sys.setenv(SMTP_PORT  = "465")              # défaut
Sys.setenv(ALERT_FROM = "email@domaine.com")
```

### Fichier .Renviron (recommandé pour automatisation)

Pour que la tâche planifiée Windows fonctionne sans intervention,
ajouter ces variables dans `C:/Users/[utilisateur]/.Renviron` :

```
SMTP_USER=email@domaine.com
SMTP_PASS=mot_de_passe
SMTP_HOST=smtp.gmail.com
SMTP_PORT=465
ALERT_FROM=email@domaine.com
```

Puis redémarrer RStudio.

---

## 8. Automatisation Windows

### Tâche planifiée
- **Nom** : PREIS_Polio_Daily
- **Déclencheur** : tous les jours à 08h00
- **Action** : `D:\PREIS_Polio_FV\run_polio_pipeline.bat`

### Fichier .bat
```bat
@echo off
cd /d D:\PREIS_Polio_FV
"C:\Program Files\R\R-4.5.2\bin\Rscript.exe" R\110_run_polio_production_pipeline.R
exit
```

### Vérifier les logs après exécution
```r
readLines("D:/PREIS_Polio_FV/outputs/logs/polio_pipeline.log")
```

---

## 9. Opérations de maintenance

### Ajouter un destinataire
Éditer `dashboard/data/dashboard/alert_recipients.csv` :
```
email,rcc,name,mode
nouveau@email.com,Eastern,Nom Prénom,SUMMARY
```
Valeurs RCC : `All`, `Northern`, `Western`, `Central`, `Eastern`, `Southern`
Valeurs mode : `SUMMARY` (10 alertes max) ou `FULL` (20 alertes max)

### Forcer l'envoi sans nouvelle issue
```r
setwd("D:/PREIS_Polio_FV")
source("R/00_load_polio_project.R")
run_polio_pipeline_if_update(force_send = TRUE)
```

### Mettre à jour manuellement le dashboard
```r
source("D:/PREIS_Polio_FV/deploy_final.R")
```

### Réinitialiser le registre last_issue
```r
# Forcer le retraitement de la prochaine issue
writeLines("", "D:/PREIS_Polio_FV/data/last_issue.txt")
```

### Tester sans envoyer d'emails
```r
Sys.setenv(PREIS_DRY_RUN = "true")
source("D:/PREIS_Polio_FV/R/110_run_polio_production_pipeline.R")
Sys.unsetenv("PREIS_DRY_RUN")
```

---

## 10. Dépendances R

```r
# Packages requis
install.packages(c(
  "shiny", "shinydashboard",
  "dplyr", "readr", "stringr", "tibble",
  "rvest", "xml2",
  "DT", "ggplot2", "plotly",
  "leaflet", "sf",
  "htmltools", "scales",
  "blastula",
  "rsconnect"
))
```

---

## 11. Résolution des problèmes fréquents

| Problème | Cause probable | Solution |
|---|---|---|
| `NO_UPDATE` tous les jours | Issue déjà traitée | Normal — attendre la prochaine publication GPEI |
| `SMTP_USER manquant` | Variables d'environnement absentes | Ajouter dans `.Renviron` |
| `0 pays africains retenus` | Page GPEI restructurée | Vérifier manuellement le site GPEI |
| Dashboard non mis à jour | Redéploiement échoué | Lancer `deploy_final.R` manuellement |
| `(0x1)` dans planificateur | Chemin .bat incorrect | Vérifier `D:\PREIS_Polio_FV` dans le .bat |

---

## 12. Historique des issues traitées

| Issue | Statut | Pays africains | RCC actifs |
|---|---|---|---|
| 08 April 2026 | ✓ Traité | — | — |
| 15 April 2026 | ✓ Traité | Namibia, South Sudan, Sudan | Eastern, Southern |
| 23 April 2026 | En attente | — | — |

---

*Documentation générée le 19 avril 2026*
*Projet PREIS_Polio_FV — Africa CDC*
