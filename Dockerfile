FROM mcr.microsoft.com/dotnet/sdk:7
WORKDIR /aoc
COPY . .
RUN dotnet restore
RUN dotnet publish --configuration Release --output output AdventOfCode.Web

FROM mcr.microsoft.com
WORKDIR /aoc
COPY --from=0 /aoc/output .
CMD dotnet AdventOfCode.Web.dll
