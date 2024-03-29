library(analogsea)
Sys.setenv(DO_PAT="eae4166ed2fac0e3c41660fe26a009bb0176ab8bceeaf753faf5189f58a06520")

temp <- analogsea::droplets()
server <- temp$`shiny-server`
analogsea::droplet_ssh(server,"rm -R /srv/shiny-server/bybecmap")
analogsea::droplet_ssh(server, "mkdir /srv/shiny-server/bybecmap")
analogsea::droplet_upload(server, "./.Renviron", "/srv/shiny-server/bybecmap/.Renviron")
analogsea::droplet_upload(server, "./app.R", "/srv/shiny-server/bybecmap/app.R")
analogsea::droplet_upload(server, "./Server", "/srv/shiny-server/bybecmap")
analogsea::droplet_upload(server, "./www", "/srv/shiny-server/bybecmap")
analogsea::droplet_upload(server, "./htmlwidgets", "/srv/shiny-server/bybecmap")
analogsea::droplet_upload(server, "./inputs", "/srv/shiny-server/bybecmap")
analogsea::droplet_ssh(server, "chown -R shiny:shiny /srv/shiny-server")
analogsea::droplet_ssh(server, "systemctl restart shiny-server")
