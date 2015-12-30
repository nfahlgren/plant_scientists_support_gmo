#!/usr/bin/python
# coding=utf-8
from __future__ import print_function
import argparse
import os
import sys
import unicodecsv as csv
import unicodedata
import codecs
import chardet

csv.field_size_limit(sys.maxsize)

# from graph_tool.all import *


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

    parser = argparse.ArgumentParser(description='Create a coauthorship network from publications downloaded '
                                                 'from the Scopus database.',
                                     formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument("-d", "--dir", help='Input directory containing CSV files from author queries '
                                            'of the Scopus database.', required=True)
    parser.add_argument("-o", "--outfile", help="Output node-edge table file.", required=True)
    parser.add_argument("-D", "--debug", help="Enable debug mode.", action='store_true')
    args = parser.parse_args()

    if not os.path.exists(args.dir):
        raise IOError("Input directory does not exist: {0}".format(args.dir))

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

    # Keep track of papers and their authors
    papers = {}

    # Keep track of authors and their papers
    coauthors = {}

    # Edges: papers that are linked by shared authors
    edges = {}

    # Query authors
    query = {}

    # Output files
    out = open(args.outfile, 'w')
    out.write(u'paper1_EID\tpaper1_citations\tpaper1_group\tpaper2_EID\tpaper2_citations\tpaper2_group\n')
    #log = open('logfile.txt', 'w')

    # Walk through the CSV directory and process coauthors from each file
    for (dirpath, dirnames, filenames) in os.walk(args.dir):
        # Each file represents one author search
        for filename in filenames:
            # Only analyze CSV files
            if filename[-3:] == 'csv':
                # Author name is the file name, minus the extension
                query_author = filename[:-4]
                query_author_names = query_author.split('_')
                if len(query_author_names) == 2:
                    query_author_lastname = query_author_names[0]
                    query_author_firstinitial = query_author_names[1]
                else:
                    query_author_lastname = ' '.join(map(str, query_author_names[:-1]))
                    query_author_firstinitial = query_author_names[-1]

                query_authorID = query_author_lastname + ' ' + query_author_firstinitial
                query_authorID = unicodedata.normalize("NFKD", unicode(query_authorID, 'utf-8'))
                # query_authorID = unicode(query_authorID.lower(), 'utf-8')

                # Open the file
                # The CSV files might be encoded as UTF-8 with BOM
                bytes = min(32, os.path.getsize(dirpath + '/' + filename))
                raw = open(dirpath + '/' + filename, 'rb').read(bytes)

                # Check to see if the file contains BOM
                if raw.startswith(codecs.BOM_UTF8):
                    # If so, set the encoding to UTF-8-Sig
                    encoding = 'utf-8-sig'
                else:
                    # If not, detect the encoding
                    result = chardet.detect(raw)
                    encoding = result['encoding']

                # csvreader = unicode_csv_reader(codecs.open(dirpath + '/' + filename, 'r', encoding=encoding))
                f = open(dirpath + '/' + filename, 'rb')
                csvreader = csv.reader(f, encoding=encoding)

                # Remove the first line header
                header = csvreader.next()

                # Table column order
                colnames = {}
                for i, col in enumerate(header):
                    colnames[col] = i

                # Read through the publications
                for publication in csvreader:
                    #print(publication)
                    # Is this a unique publication?
                    articleID = publication[colnames['EID']]
                    # log.write(u' '.join(('Author name:', query_authorID, ',
                    # PaperID:', articleID + '\n')).encode('utf-8'))

                    if articleID in papers:
                        # We have already processed this article before
                        continue
                    else:
                        # Make sure the query author was found
                        author_found = False

                        query[articleID] = dirpath

                        # Register paper
                        #print('\t'.join(publication) + '\n')
                        papers[articleID] = {}
                        if publication[colnames['Cited by']]:
                            papers[articleID]['citations'] = int(publication[colnames['Cited by']])
                        else:
                            papers[articleID]['citations'] = 0

                        # Get list of authors for the publication
                        auth_list = []
                        # Authors are split by a period from the previous initial, a comma and a space
                        # Join hyphenated initials
                        publication[colnames['Authors']] = publication[colnames['Authors']].replace('.-', '-')
                        # Remove periods from suffixes
                        publication[colnames['Authors']] = publication[colnames['Authors']].replace('Jr.,', 'Jr,')

                        authors = publication[colnames['Authors']].split('., ')

                        for author in authors:
                            # author = author.lower()
                            if args.debug:
                                print(articleID + ': ' + author)
                            lastname, firstnames = author.split(', ')
                            if '-' in firstnames:
                                firstinitial = firstnames[0:3]
                            else:
                                firstinitial = firstnames[0]

                            authID = lastname + ' ' + firstinitial
                            authID = unicodedata.normalize("NFKD", authID)

                            # Make sure the query author was found
                            if authID == query_authorID:
                                author_found = True

                            auth_list.append(authID)

                        # Add the paper to the index of papers for each author
                        # And add each author to the index of authors for the paper
                        for author in auth_list:
                            if author in coauthors:
                                coauthors[author][articleID] = 1
                            else:
                                coauthors[author] = {}

                            papers[articleID][author] = 1

                    if author_found is False:
                        print(u': '.join((u'ERROR', query_authorID, articleID)).encode('utf-8'))

    # Loop through each paper
    for paper in papers.keys():
        # For each coauthor on the paper, loop through all their other papers
        for author in papers[paper].keys():
            # Ignore the citations key
            if author != 'citations':
                # For each paper an author has, define paper1-paper2 edge, if it is unique
                for author_paper in coauthors[author].keys():
                    # Define the pair of papers
                    pair = [paper, author_paper]
                    # Sort the paper IDs so that we only check for this pair in one order
                    pair.sort()
                    edge = ':'.join(pair)
                    # Is this a new edge?
                    if edge not in edges:
                        edges[edge] = query[paper]

    for edge in edges:
        paper1, paper2 = edge.split(':')
        out.write(paper1 + '\t' + str(papers[paper1]['citations']) + '\t' + query[paper1] + '\t' +
                  paper2 + '\t' + str(papers[paper2]['citations']) + '\t' + query[paper2] + '\n')

    # # Output d3 graph
    # paper_index = {}
    # paper_id = 0
    # d3 = open('network.json', 'w')
    # d3.write('{\n')
    # d3.write('\t"nodes":[\n')
    #
    # for paper in papers.keys():
    #     paper_index[paper] = paper_id
    #     paper_id += 1
    #     d3.write(u''.join(('\t\t{"name":"' + paper + '","group":1},\n')).encode('utf-8'))
    #
    # d3.write('\t],\n')
    # d3.write('\t"links":[\n')
    #
    # for edge in edges.keys():
    #     paper1, paper2 = edge.split(':')
    #     d3.write(u''.join(('\t\t{"source":' + str(paper_index[paper1]) +
    #                        ',"target":' + str(paper_index[paper2]) +
    #                        ',"value":1},\n')).encode('utf-8'))
    #
    # d3.write('\t]\n')
    # d3.write('}\n')

    # d3.write('<!DOCTYPE html>\n')
    # d3.write('<meta charset="utf-8">\n')
    # d3.write('<style>\n\n')
    # d3.write('.node {\n')
    # d3.write('\tstroke: #fff;\n')
    # d3.write('\tstroke-width: 1.5px;\n')
    # d3.write('}\n\n')
    # d3.write('.link {\n')
    # d3.write('\tstroke: #999;\n')
    # d3.write('\tstroke-opacity: .6;\n')
    # d3.write('}\n\n')
    # d3.write('</style>\n\n')
    # d3.write('<body>\n')
    # d3.write('<script src="//d3js.org/d3.v3.min.js"></script>\n')
    # d3.write('<script>\n\n')
    # d3.write('var width = 960, height = 500;\n\n')
    # d3.write('var color = d3.scale.category20();\n\n')
    # d3.write('var force = d3.layout.force()\n')
    # d3.write('\t.charge(-120)\n')
    # d3.write('\t.linkDistance(30)\n')
    # d3.write('\t.size([width, height]);\n\n')
    # d3.write('var svg = d3.select("body").append("svg")\n')
    # d3.write('\t.attr("width", width)\n')
    # d3.write('\t.attr("height", height);\n\n')
    # d3.write('')
    # d3.write('</script>\n\n')


###########################################

# Unicode CSV reader
###########################################
def unicode_csv_reader(utf8_data):
    csvreader = csv.reader(utf8_data)
    for row in csvreader:
        yield [unicode(cell, 'utf-8') for cell in row]


###########################################

if __name__ == '__main__':
    main()
