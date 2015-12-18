#!/usr/bin/python
# coding=utf-8
from __future__ import print_function
import argparse
import os
import unicodecsv as csv
import unicodedata
import codecs
import chardet
from graph_tool.all import *


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

    # Keep track of papers we have processed
    papers = {}

    # Co-author network
    coauthors = {}

    # Author publications
    pubs = {}

    # Output files
    out = open(args.outfile, 'w')
    out.write(u'Author1_name\tAuthor1_pubs\tAuthor2_name\tAuthor2_pubs\tCoauthorships\n')
    log = open('logfile.txt', 'w')

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
                #query_authorID = unicode(query_authorID.lower(), 'utf-8')

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

                #csvreader = unicode_csv_reader(codecs.open(dirpath + '/' + filename, 'r', encoding=encoding))
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
                    # Is this a unique publication?
                    articleID = publication[colnames['EID']]
                    #log.write(u' '.join(('Author name:', query_authorID, ', PaperID:', articleID + '\n')).encode('utf-8'))

                    if articleID in papers:
                        continue
                    else:
                        # Register the paper
                        papers[articleID] = 1

                        # Make sure the query author was found
                        author_found = False

                        # Get list of authors for the publication
                        auth_list = []
                        # Authors are split by a period from the previous initial, a comma and a space
                        # Join hyphenated initials
                        publication[colnames['Authors']] = publication[colnames['Authors']].replace('.-', '-')
                        # Remove periods from suffixes
                        publication[colnames['Authors']] = publication[colnames['Authors']].replace('Jr.,', 'Jr,')

                        authors = publication[colnames['Authors']].split('., ')
                        authors.sort()
                        for author in authors:
                            #author = author.lower()
                            #print(articleID + ': ' + author)
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
                            else:
                                if lastname == u'román':
                                    print(query_authorID.encode('utf-8'))

                            auth_list.append(authID)
                            if authID in pubs:
                                pubs[authID] += 1
                            else:
                                pubs[authID] = 1

                        # Build a list of unique coauthor interactions
                        for i, author in enumerate(auth_list):
                            for j in range(i + 1, len(auth_list)):
                                # Have we seen this interaction before?
                                if auth_list[i] + ':' + auth_list[j] in coauthors:
                                    coauthors[auth_list[i] + ':' + auth_list[j]] += 1
                                else:
                                    coauthors[auth_list[i] + ':' + auth_list[j]] = 1

                    if author_found == False:
                        print(u': '.join((u'ERROR', query_authorID, articleID)).encode('utf-8'))

    for key in coauthors.keys():
        author1, author2 = key.split(':')
        out.write(u'\t'.join(
            (author1, str(pubs[author1]), author2, str(pubs[author2]), str(coauthors[key]) + '\n')).encode('utf-8'))




    # Output d3 graph
    # auth_index = {}
    # aid = 0
    # d3 = open('coauthors.json', 'w')
    # d3.write('{\n')
    # d3.write('\t"nodes":[\n')
    #
    # for author in pubs.keys():
    #     auth_index[author] = aid
    #     aid += 1
    #     d3.write(u''.join(('\t\t{"name":"' + author + '","group":1},\n')).encode('utf-8'))
    #
    # d3.write('\t],\n')
    # d3.write('\t"links":[\n')
    #
    # for key in coauthors.keys():
    #     author1, author2 = key.split(':')
    #     d3.write(u''.join(('\t\t{"source":' + str(auth_index[author1]) +
    #                        ',"target":' + str(auth_index[author2]) +
    #                        ',"value":' + str(coauthors[key]) + '},\n')).encode('utf-8'))
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
