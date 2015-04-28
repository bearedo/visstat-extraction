# How to connect to the visstat database #

The following points guide you through the process to access the visstat database from within R. For a SAS connection, please refer to Sieto Verver or Peter van der Kamp.

## Official access to visstat ##
  * You need to have a 'raja' account. Most people working in IJmuiden do have one. If not, consult Peter van der Kamp to get one.

  * Before you are allowed to access the visstat database, you need to ask permission and have a form signed by CVO. Only when this form is signed, and handed over to Peter van der Kamp, access can be granted to access the visstat database

## Computer settings ##
  * Assuming your PC has Windows 7 installed, you need to configure your ODBC settings. To be able to configure these, you need to have `OraClient11g_home1` installed, which is available under _Available Software_ provided by WUR.
  * Once installed, browse through start to find the _Oracle_ folder including the sub-folder _Configuration and Migration Tools_. Here, a link can be found to the microsoft ODBC administrator.
  * Under tab _User DSN_ you can add a source.
    * `Data Source Name`: visstatp
    * `Description`: visstatp
    * `TNS Service Name`: visstatp
    * `User ID`: _your raja username_
  * Use the _Test Connection_ button and fill out your visstat password to see if the connection works