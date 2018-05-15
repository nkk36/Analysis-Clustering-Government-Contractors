# Research & Analysis: Clustering Government Contractors

This is a repository for my ongoing research and analysis clustering government contractors using data from USA Spending. I will update as I make progress. The goal is to maintain a reproducible workflow.

## Getting Started

These instructions will get you a copy of the project up and running on your local machine to reproduce my analyses. Below you will find a list of packages and their versions used.

### Prerequisites

Here is a list of packages which you will need to run the application. You will also need all of their package dependencies as well. **This analysis is being performed using R 3.4.3.**

| Packages        | Version   |
| --------------- |:---------:|
| R Package       | Version # |

### Data

Data was downloaded for fiscal year 2016 from [USASpending.gov](https://www.usaspending.gov). The data archive used to download the entire data set (~ 10GB) can be found [here](https://www.usaspending.gov/DownloadCenter/Pages/dataarchives.aspx). I saved the data into a local PostgreSQL database and before pulling it into R I removed all unnecessary columns and saved what I needed into its own table which significantly reduced the size (~ 1 GB).

<!---

```
Give examples
```

### Installing

A step by step series of examples that tell you have to get a development env running

Say what the step will be

```
Give the example
```

And repeat

```
until finished
```

End with an example of getting some data out of the system or using it for a little demo

## Running the tests

Explain how to run the automated tests for this system

### Break down into end to end tests

Explain what these tests test and why

```
Give an example
```

### And coding style tests

Explain what these tests test and why

```
Give an example
```

## Deployment

Add additional notes about how to deploy this on a live system
-->

## Built With

* [R](https://www.r-project.org/) - statistical computing software
* [Packrat](https://rstudio.github.io/packrat/) - R package for dependency management

## Contributing

If you'd like to contribute you can email me at nkallfa36@gmail.com.
<!---
## Versioning

We use [SemVer](http://semver.org/) for versioning. For the versions available, see the [tags on this repository](https://github.com/your/project/tags).

## Authors

* **Nicholas Kallfa**
## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

## Acknowledgments

* Hat tip to anyone who's code was used
* Inspiration
* etc
-->
