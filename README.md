# MNWD water usage optimization

## app

Data lives in the Dropbox and can be accessed locally or remotely.

### Local set up

Add the following lines to your .Renviron (create ~/.Renviron if needed).

    MNWD_ENV=test
    MNWD_DATA_DIR=/path/to/your/Dropbox/mnwd_dk/Data

### Remote setup

As `sudo su shiny`, add the following to ~/.Renviron.

    MNWD_ENV=prod
    MNWD_DATA_DIR=/svr/shiny-server/data
