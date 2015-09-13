#!/usr/bin/env python

#For update:
# - For each bib entry in master bib, check since last master bib
#  update:
#  - If pdf updated: re-run get annotations
#  - If notes updated: update master notes
#  - If bib updated: update master bib

# Decide if I want to make the master bib and org files unable for me
# to edit or try and find a way to merge any edits. There's a python
# difflib that would help with this. -- I think for now, make the
# "master" files un-writable, and just update them to reflect the
# changes in the sub-files.

#Also need a function for adding a file to library. That should
#intialize the note file (move org_format to new script, have
#mendeley_to_org grab it from there), make directory, rename pdf and
#move it there. Require bib to be downloaded as well or get that
#automatically? Hard without doi, and getting a doi automatically not
#guaranteed.  Can try to get doi like so:
"""
regex=re.compile('\\b(10[.][0-9]{4,}(?:[.][0-9]+)*/(?:(?![\"&\'<>])\S)+)\\b')

doc = poppler.document_new_from_file(path,None)
pages = [doc.get_page(i) for i in range(doc.get_n_pages())]
doi = []
for page in pages:
    text = page.get_text()
    doi += re.findall(regex,text)
#check they're all the same and peace out
"""
#If got doi, then just do the following. Else, give option to enter doi, then tell user to manually download bib.

#With pybib installed, can just run `bib DOI` from the command line and get its bib file. Can run from within python though?
#Actually, just use the code that underlies it, found at https://github.com/jgilchrist/pybib/blob/master/pybib/utils.py


def update(fill_column=70):
    import os,stat,bibtexparser,time,re
    from mendeley_to_org import paper_dir,org_format
    
    #Make it so we can write again...
    os.chmod(paper_dir+'/literature.bib',stat.S_IWUSR|stat.S_IREAD)
    os.chmod(paper_dir+'/literature.org',stat.S_IWUSR|stat.S_IREAD)
    
    if paper_dir[-1]=='/':
        paper_dir=paper_dir[:-1]
    
    with open(paper_dir+'/literature.bib') as f:
        bib_db = bibtexparser.loads(f.read())

    modified_time = os.path.getmtime(paper_dir+'/literature.bib')
    assert modified_time==os.path.getmtime(paper_dir+'/literature.bib'),'Master bib and org files edited separately somehow!'
    for bib in bib_db.entries:
        dir_path = os.path.expanduser(os.path.split(bib['file'])[0])
        bib_id = bib['ID']
        if os.path.getmtime('%s/%s.pdf'%(dir_path,bib_id)) > modified_time:
            annots = get_annotations('%s/%s.pdf'%(dir_path,bib_id),'org',org_indent=re.search('Annotations *\n( *){annotations}',org_format).groups()[0],fill_column=fill_column)
            with open('%s/%s.org'%(dir_path,bib_id),'r+') as f:
                org_note = f.read()
                org_note = re.sub("Annotations.*\n([*]+)","Annotations\n\n%s\n\n%s"%(annots,r"\1"),org_note,flags=re.DOTALL)
                f.seek(0)
                f.write(org_note)
                f.truncate()
        if os.path.getmtime('%s/%s.org'%(dir_path,bib_id)) > modified_time:
            #Replace section in master org with new local org (use regex?)
            'hi'
        if os.path.getmtime('%s/%s.bib'%(dir_path,bib_id)) > modified_time:
            #Replace section in master bib with new local bib (use regex?)
            'bye'
            
            
    #We want these files to be read-only so that the only edits are to
    #the individual org and bib files. If this is too inconvenient,
    #may end up changing it.
    os.chmod(paper_dir+'/literature.bib',stat.S_IREAD|stat.S_IRGRP|stat.S_IROTH)
    os.chmod(paper_dir+'/literature.org',stat.S_IREAD|stat.S_IRGRP|stat.S_IROTH)
            

def get_annotations(path,note_format='plain',org_indent='   ',fill_column=70):
    # Based on code from Steve Powell, found at
    # (https://gist.github.com/stevepowell99/e1e389a57ea9a2bcb988).
    # See his blog post
    # http://socialdatablog.com/extract-pdf-annotations.html for more
    # details
    
    #note_format, one of:
    #   plain -- no special formatting, each annotation on separate line
    #   wrap -- annotation wrapped so that each has a new line after fill_column characters
    #   org -- for .org notes file, wrapped after fill_column characters, each line starts with org_indent
    
    import poppler, os.path, os, time, datetime, re

    assert note_format in ['plain','wrap','org'],"Don't know how to format annotations with note_format %s"%note_format
    
    notes=''	
    # print("Found PDF:..."+fullpath)
    # print "Size: ... "+os.stat(fullpath).st_size
    path = "file://%s"%path
    try:
        doc = poppler.document_new_from_file(path, None)
    except:
        print "some pdf problem"
    else:
        pages = [doc.get_page(i) for i in range(doc.get_n_pages())]
        for page_no, page in enumerate(pages):
            items = [i.annot.get_contents() for i in page.get_annot_mapping()]
            items = [i for i in items if i]
            for j in items:
                #Every so often, we'll have an annotation with (1 or
                #more) newlines in it; we want to get rid of them. If
                #a line ends with a letter or number then it's the end
                #of a sentence, so add a period there.
                j = re.subn("([a-zA-Z0-9])\n",r"\1. ",j)[0].replace("\n"," ")
                if note_format=='plain':
                    notes= notes+'%s (page %s)\n\n' % (j,page_no + 1)
                elif note_format=='wrap':
                    j = col_wrap('%s (page %s)'%(j,page_no+1),fill_column)
                    notes=notes+'%s\n\n'%j
                elif note_format == 'org':
                    j = col_wrap('%s (page %s)'%(j,page_no+1),fill_column,org_indent)
                    notes=notes+'%s%s\n\n'%(org_indent,j)                    
        return notes

def col_wrap(text,fill_col,org_indent=''):
    text = text.split(' ')
    length = 0
    for idx,i in enumerate(text):
        if length+len(i) > fill_col:
            text[idx] = i+"\n%s"%org_indent
            length=0
        else:
            length+=len(i)
    text = ' '.join(text).replace("\n ","\n")
    return text
    
