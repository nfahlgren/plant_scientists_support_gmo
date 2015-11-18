#!/usr/bin/python
from __future__ import print_function
import argparse
import os
import requests
from xml.etree import ElementTree


# Parse command-line arguments
###########################################
def options():
    """Parse command line options.

    Args:

    Returns:
        argparse object.
    Raises:
        IOError: if input file does not exist.
    """

    parser = argparse.ArgumentParser(description="Pubmed author query tool.",
                                     formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument("-f", "--file", help="Input file containing list of author queries.", required=True)
    parser.add_argument("-o", "--outfile", help="Output file.", required=True)
    args = parser.parse_args()

    if not os.path.exists(args.file):
        raise IOError("Input file does not exist: {0}".format(args.file))

    return args


###########################################

# Main
###########################################
def main():
    """Main program.

    Args:

    Returns:

    Raises:

    """

    # Get options
    args = options()

    # Variables
    url_base = 'http://eutils.ncbi.nlm.nih.gov/entrez/eutils/'
    esearch = 'esearch.fcgi'
    efetch = 'efetch.fcgi'

    # Open output file
    output = open(args.outfile, 'w')

    # Open author list file
    authors = open(args.file, 'r')
    for author in authors:
        author = author.rstrip('\n')
        author = author.replace(" ", "%20")
        author_url = url_base + esearch + "?db=pubmed&term=%22" + author + "%22[author]&retmax=9999"
        author_results = requests.get(author_url)
        author_xml = ElementTree.fromstring(author_results.content)
        pubmed_ids = author_xml[3]
        for id in pubmed_ids:
            paper_url = url_base + efetch + "?db=pubmed&retmode=xml&id=" + id.text
            paper_result = requests.get(paper_url)
            print(id.text)


###########################################


if __name__ == '__main__':
    main()
