% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api.R
\name{set_trello_auth_infos}
\alias{set_trello_auth_infos}
\title{Set Trello's auth-infos}
\usage{
set_trello_auth_infos(force_update = FALSE)
}
\arguments{
\item{force_update}{Boolean. Specify whether to forcibly update the authentication information.}
}
\value{
This function does not return the object, but records the credentials directly locally.
}
\description{
This is a function that sets Trello's credentials (key / token).
The first time you call this function, it will use the keying package
to register the key / token information on your local machine.
Once the key / token information is registered in the local machine,
the functions in this package will automatically acquire
and use the authentication information in the local machine.
If you want to execute batch processing etc. on the virtual machine,
set key / token in the argument of each API functions
instead of registering the authentication information with this function.
}
