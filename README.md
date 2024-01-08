# Atles de l'Habitatge

L'Atles de l'Habitatge és un projecte del Sindicat de Llogateres que pretén recollir les dades disponibles públicament relacionades amb l'habitatge a Catalunya.


## Directoris i fitxers:

-   `_targets.R`: És el fitxer índex del projecte targets. Conté la definició del pipeline.
-   `R/*_functions.R`: Conté les funcions, el codi específic per formar cada un dels nodes del pipeline.
-   `data/`: Directori que conté les dades inicials que es fan servir com a punt de partida del projecte sempre que no es puguin aconseguir d'una URL de tercers. Futurament es migrarà en un repositori extern tipus S3.
-   `ingesta/`: Conté els scripts d'obtenció de dades per les fonts en què sigui necessari, és a dir, que no es puguin obtenir les dades amb un script d'R simple.
-   `docs/`: Conté els fitxers de documentació del projecte.
-   `renv.lock`: Fitxer generat automàticament per Renv que conté la llista de paquests necessaris per executar el projecte. Aquest fitxer és el que llegeix la comanda `renv::restore()` Per crear l'entron d'R.
-   `.gitignore` Fitxer que utilitza GIT per saber quins fitxers ha d'ignorar, és a dir no pujar al repositori quan guardes el codi.


## Setup

Per configurar i començar a utilitzar aquest projecte, segueix els passos següents:

1.  **Assegura't que tens instal·lat prèviament l'Renv**:

    ``` r
    install.packages(’renv’)
    ```

2.  **Clonar el repositori:** Clona el repositori utilitzant Git amb la comanda següent:

    ``` bash
    git clone git@github.com:catbru/atles_habitatge.git
    ```

    O bé configurant el repositori des de R Studio mateix.

3.  **Crear un projecte d'R Studio:** Utilitzant R Studio ves al directori que acabes de clonar i crea un nou projecte d'R Studio utilitzant l'opció 'using Renv'.

4.  **Activar renv:** En R Studio, obre la consola i activa l'entorn de renv amb:

    ``` r
    renv::activate()
    ```

5.  **Restaurar les dependències:** Encara en la consola, restaura les dependències del projecte amb:

    ``` r
    renv::restore()
    ```

6.  **Visualitzar la xarxa de dependències dels targets:** Per entendre millor com estan interconnectats els diferents components del projecte, pots visualitzar la xarxa de dependències dels targets amb:

    ``` r
    targets::tar_visnetwork(targets_only = TRUE)
    ```

    El paràmetre `targets_only = TRUE` fa que no presenti les funcions al gràfic, cosa que el fa molt més entenedor si el que volem és representar els diferents passos de transformació.

    Els nodes apareixen de color verd quan Targets té el resultat guardat. SI fem una modificació de la funció veurem que canvia el color fins que no fem `tar_make()`.

7.  **Executar el pipeline:** Finalment, per executar el pipeline complet del projecte, utilitza la següent comanda:

    ``` r
    targets::tar_make()
    ```

8.  **Visualitza el resultat** de qualsevol node en concret

    Podem veure el resultat o guardar-lo en una variable de qualsevol punt (node) del pipeline utilitzant la comanda `tar_read()`. Per exemple per aconseguir l'històric de preus de totes les localitzacions disponibles.

    ``` r
    prices <- targets::tar_read('locations_with_price_evolution')
    prices
    ```
