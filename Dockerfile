#FROM microsoft/dotnet:2.2-aspnetcore-runtime-alpine
#FROM microsoft/dotnet:2.2-runtime-alpine
FROM microsoft/dotnet:2.2-runtime-deps-alpine
COPY /deploy /
WORKDIR /Server
EXPOSE 8085
ENV "DB" "/data/simple.db"
ENV APPINSIGHTS_INSTRUMENTATIONKEY=""
#RUN apk add --no-cache bash
#ENTRYPOINT [ "/bin/bash" ]
ENTRYPOINT [ "./Server" ]
#ENTRYPOINT [ "dotnet", "Server.dll" ]