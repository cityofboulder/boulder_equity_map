<div id="top"></div>
<!--
*** From https://github.com/othneildrew/Best-README-Template
*** Thanks for checking out the Best-README-Template. If you have a suggestion
*** that would make this better, please fork the repo and create a pull request
*** or simply open an issue with the tag "enhancement".
*** Don't forget to give the project a star!
*** Thanks again! Now go create something AMAZING! :D
-->



<!-- PROJECT SHIELDS -->
<!--
*** I'm using markdown "reference style" links for readability.
*** Reference links are enclosed in brackets [ ] instead of parentheses ( ).
*** See the bottom of this document for the declaration of the reference variables
*** for contributors-url, forks-url, etc. This is an optional, concise syntax you may use.
*** https://www.markdownguide.org/basic-syntax/#reference-style-links
-->
<!-- [![Contributors][contributors-shield]][contributors-url] -->
<!-- [![Forks][forks-shield]][forks-url] -->
<!-- [![Stargazers][stars-shield]][stars-url] -->
[![Issues][issues-shield]][issues-url]
[![License][license-shield]][license-url]



<!-- PROJECT LOGO -->
<br />
<div align="center">
  <a href="https://github.com/github_username/repo_name">
    <img src="images/boulder-logo.png" alt="Logo" width="160" height="160">
  </a>

<h3 align="center">City of Boulder Racial Equity Map (WIP)</h3>

  <p align="center">
    A map combining variables from the American Community Survey 5-year estimates (2016-2020) with city data to investigate demographic and economic differences across census block groups. (Target completion date: November 2022)
    <br />
    <!-- <a href="https://github.com/github_username/repo_name"><strong>Explore the docs »</strong></a>
    <br /> -->
    <br />
    <!-- <a href="https://github.com/github_username/repo_name">View Demo</a> -->
    <!-- · -->
    <a href="https://github.com/cityofboulder/boulder_equity_map/issues">Report Bug</a>
    ·
    <a href="https://github.com/cityofboulder/boulder_equity_map/issues">Request Feature</a>
  </p>
</div>



<!-- TABLE OF CONTENTS -->
<details>
  <summary>Table of Contents</summary>
  <ol>
    <li>
      <a href="#about-the-project">About The Project</a>
      <ul>
        <li><a href="#built-with">Built With</a></li>
      </ul>
    </li>
    <li>
      <a href="#getting-started">Getting Started</a>
      <ul>
        <li><a href="#prerequisites">Prerequisites</a></li>
        <li><a href="#installation">Installation</a></li>
      </ul>
    </li>
    <li><a href="#usage">Usage</a></li>
    <li><a href="#roadmap">Roadmap</a></li>
    <li><a href="#contributing">Contributing</a></li>
    <li><a href="#license">License</a></li>
    <li><a href="#contact">Contact</a></li>
    <li><a href="#acknowledgments">Acknowledgments</a></li>
  </ol>
</details>



<!-- ABOUT THE PROJECT -->
## About The Project

This project leverages data from the American Community Survey 5-year estimates (2016-2020) and point data for aid recipients documented by Housing and Human Services to develop a racial equity index map for the City of Boulder. The index provides a simplified, 5-level ranking of census block groups in Boulder based on a combination of economic and demographic variables.

<!-- * % of block group population that are people of color (POC)
* % of block group population that are Hispanic/Latino
* Median income by block group
* % of block group population living below the poverty line
* % of block group population receiving financial aid from Housing and Human Services programs.  -->

<p align="right">(<a href="#top">back to top</a>)</p>



### Built With

* [R](https://www.r-project.org/)

<p align="right">(<a href="#top">back to top</a>)</p>



<!-- GETTING STARTED -->
## Getting Started

### Prerequisites

<!-- This is an example of how to list things you need to use the software and how to install them. -->
* R version 4.2.0 or greater
* RStudio 2022.02.3 Build 492 or greater


### Installation

1. Clone the repo
   ```sh
   git clone https://github.com/cityofboulder/boulder_equity_map
   ```
2. Install packages
   ```sh
   install.packages("tidyverse")
   install.packages("stringr")
   install.packages("ggplot2")
   install.packages("tidygeocoder")
   install.packages("lubridate")
   install.packages("sf")
   install.packages("tidycensus")
   install.packages("tigris")
   install.packages("rosm")
   install.packages("tmap")
   install.packages("tmaptools")
   install.packages("OpenStreetMap")
   install.packages("data.table")
   ```
3. Get free Geocodio API at https://www.geocod.io/docs/#authentication.
4. Request a Census API key at https://api.census.gov/data/key_signup.html and add it to your environment as follows:
   ```sh
   library(tidycensus)
   census_api_key("YOUR API KEY GOES HERE")
   ```
5. For privacy reasons, the raw Parks and Recreation and Housing and Human Services client data are not included in this repository. for reference, the file names are:
  * <b>pr_financial_aid.csv</b> - Parks and Recreation financial aid records from 2019 (as 2020-21 enrollment may have been suppressed by the COVID-19 pandemic)
  * <b>HHS Case Records.csv</b> - Housing and Human Services individual enrollment records (2020)
  * <b>affordable_housing_units_geocoded_cleaned.csv</b> - Housing and Human Services affordable housing units within city limits (2020)
6. The file <b>geocorr_boulder_city.csv</b>, used in 1_ACS_variable_selection.R to filter for Boulder's Census block groups, was generated via the Missouri Census Data Center's <a href="https://mcdc.missouri.edu/applications/geocorr2022.html">Geographic Correspondence Engine</a> and edited to include block groups for Boulder sub-communities that are not officially annexed by the city.

The repository is organized as follows:

* The <b>data</b> folder contains subfolders for raw input data (except sensitive data mentioned above) and cleaned outputs.
* The <b>data_preparation</b> folder contains all scripts necessary to convert the raw data to cleaned products. 
* The <b>EDA</b> folder contains raw, exploratory code that is unnecessary for construction of the index, but may provide further insight into the data used.
* The <b>index_construction</b> folder contains the script that compiles the data sources into the equity index.
* The <b>outputs</b> folder contains images, maps, figures, and written products generated for this analysis.

<p align="right">(<a href="#top">back to top</a>)</p>



<!-- USAGE EXAMPLES -->
## Usage
<!-- 
Use this space to show useful examples of how a project can be used. Additional screenshots, code examples and demos work well in this space. You may also link to more resources. -->

<!-- _For more examples, please refer to the [Documentation](https://example.com)_ -->


This index is designed to provide an informative (but not predictive) view of racial and economic distributions within Boulder, with higher ranks on the index indicating an area of greater potential need. The index is comprised of the following variables:

* % of the block group population that are people of color ("POC")
* % of the block group population that are Hispanic/Latino
* Median income by block group
* % of the block group population living below the poverty line
* % of the block group population receiving financial aid through Housing and Human Services ("HHS") aid programs

These variables were combined as a weighted sum, with race and ethnicity variables double-weighted, then broken into 5 ranks using Jenks natural breaks, with a rank of 5 representing areas of highest priority. 

<p align="center"><img src="figures/tidy_figures/Equity Index.png" alt="City of Boulder Racial Equity Map" ></p>

Racial equity is far more complex than this index is able to reflect; as such, it is intended as a starting point. It is meant to highlight racial equity concerns in conjunction with other data layers for project planning and resource allocation. In situations where all other variables are equal, it may be used as a "tie breaker" for decision makers.

<p align="right">(<a href="#top">back to top</a>)</p>



<!-- ROADMAP -->
## Roadmap

<!-- - [ ] Feature 1
- [ ] Feature 2
- [ ] Feature 3
    - [ ] Nested Feature -->

<!-- See the [open issues](https://github.com/cityofboulder/boulder_equity_map/issues) for a full list of proposed features (and known issues). -->

<p align="right">(<a href="#top">back to top</a>)</p>



<!-- CONTRIBUTING -->
## Contributing

<!-- Contributions are what make the open source community such an amazing place to learn, inspire, and create. Any contributions you make are **greatly appreciated**.
 -->
If you have a suggestion that would make this better, please fork the repo and create a pull request. You can also simply open an issue with the tag "enhancement".
<!-- Don't forget to give the project a star! Thanks again! -->

1. Fork the Project
2. Create your Feature Branch (`git checkout -b feature/NewFeature`)
3. Commit your Changes (`git commit -m 'Add some NewFeature'`)
4. Push to the Branch (`git push origin feature/NewFeature`)
5. Open a Pull Request

<p align="right">(<a href="#top">back to top</a>)</p>



<!-- LICENSE -->
## License

Distributed under the MIT License. See `LICENSE.txt` for more information.

<p align="right">(<a href="#top">back to top</a>)</p>



<!-- CONTACT -->
## Contact

Stewart LaPan - lapans@bouldercolorado.gov

Project Link: [https://github.com/cityofboulder/boulder_equity_map](https://github.com/cityofboulder/boulder_equity_map)

<p align="right">(<a href="#top">back to top</a>)</p>



<!-- ACKNOWLEDGMENTS -->
## Acknowledgments

* Christy Spielman
* Nicolia Eldred-Skemp
* Richard Todd
* Aimee Kane
* Ana Silvia Avendano-Curiel

<p align="right">(<a href="#top">back to top</a>)</p>



<!-- MARKDOWN LINKS & IMAGES -->
<!-- https://www.markdownguide.org/basic-syntax/#reference-style-links -->
<!-- [contributors-shield]: https://img.shields.io/github/contributors/github_username/repo_name.svg?style=for-the-badge
[contributors-url]: https://github.com/github_username/repo_name/graphs/contributors
[forks-shield]: https://img.shields.io/github/forks/github_username/repo_name.svg?style=for-the-badge
[forks-url]: https://github.com/github_username/repo_name/network/members
[stars-shield]: https://img.shields.io/github/stars/github_username/repo_name.svg?style=for-the-badge -->
<!-- [stars-url]: https://github.com/github_username/repo_name/stargazers -->
[issues-shield]: https://img.shields.io/github/issues/cityofboulder/boulder_equity_map.svg?style=for-the-badge
[issues-url]: https://github.com/cityofboulder/boulder_equity_map/issues
[license-shield]: https://img.shields.io/badge/License-MIT-yellow.svg
[license-url]: https://github.com/github_username/repo_name/blob/master/LICENSE.txt