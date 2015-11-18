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

    # Unique PubMed ID list
    pubmed_ids = {}

    # Open author list file
    authors = open(args.file, 'r')
    # Query PubMed with each author name
    for author in authors:
        author = author.rstrip('\n')
        author = author.replace(" ", "%20")
        author_url = url_base + esearch + "?db=pubmed&term=%22" + author + "%22[author]&retmax=9999"
        author_results = requests.get(author_url)
        author_xml = ElementTree.fromstring(author_results.content)
        author_pubmed_ids = author_xml[3]
        for id in author_pubmed_ids:
            pubmed_ids[id.text] = 1

    # Fetch all papers
    paper_url = url_base + efetch + "?db=pubmed&retmode=xml&id=" + ','.join(map(str, pubmed_ids.keys()))
    paper_results = requests.get(paper_url)
    paper_xml = ElementTree.fromstring(paper_results.content)
    # Get articles from results
    for article in paper_xml:
        citation = article[0]
        for item in citation:
            if item.tag == 'Article':
                for element in item:
                    if element.tag == 'AuthorList':
                        for author_name in element:
                            lastname = author_name[0].text
                            firstname = author_name[1].text
                            #initials = author_name[2].text
                            print(firstname + ' ' + lastname)
        #article_info = citation[3]
        #print(article_info)



###########################################


if __name__ == '__main__':
    main()
