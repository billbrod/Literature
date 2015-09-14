#!/usr/bin/env python

#For update:
# - For each bib entry in master bib, check since last master bib
#  update:
#  - If pdf updated: re-run get annotations
#  - If notes updated: update master notes
#  - If bib updated: update master bib

#This file only updates one-way: from local files to master files. For
#this reason, the master files are kept read-only. If this becomes too
#annoying, I may try to find a way to update both ways, but for now
#this works.

def update(fill_column=70):
    import os,stat,bibtexparser,time,re
    from lit_add import paper_dir,org_format
    
    #Make it so we can write again...
    os.chmod(paper_dir+'/literature.bib',stat.S_IWUSR|stat.S_IREAD)
    os.chmod(paper_dir+'/literature.org',stat.S_IWUSR|stat.S_IREAD)
    
    if paper_dir[-1]=='/':
        paper_dir=paper_dir[:-1]
    
    with open(paper_dir+'/literature.bib') as f:
        bib_db = bibtexparser.load(f)

    modified_time = {'bib':os.path.getmtime(paper_dir+'/literature.bib'),'org':os.path.getmtime(paper_dir+'/literature.org')}
    for bib in bib_db.entries:
        bib_id = bib['ID']
        if 'file' in bib: 
            dir_path = os.path.expanduser(os.path.split(bib['file'])[0])
        else:             
            dir_path = paper_dir+'/%s'%bib_id
        if os.path.isfile('%s/%s.pdf'%(dir_path,bib_id)) and os.path.getmtime('%s/%s.pdf'%(dir_path,bib_id)) > modified_time['org']:
            print('Pdf %s.pdf updated, extracting annotations'%bib_id)
            annots = get_annotations('%s/%s.pdf'%(dir_path,bib_id),'org',org_indent=re.search('Annotations *\n( *){annotations}',org_format).groups()[0],fill_column=fill_column)
            with open('%s/%s.org'%(dir_path,bib_id),'r+') as f:
                org_note = f.read()
                re_sub = re.subn("Annotations.*\n([*]+)","Annotations\n\n%s\n\n%s"%(annots,r"\1"),org_note,flags=re.DOTALL)
                assert re_sub[1]==1,'Thought pdf was updated but no (or more than one...) substitution made...'
                f.seek(0)
                f.write(re_sub[0])
                f.truncate()
        if os.path.getmtime('%s/%s.org'%(dir_path,bib_id)) > modified_time['org']:
            print('Org file %s.org updated, copying new changes to master org'%bib_id)
            with open('%s/%s.org'%(dir_path,bib_id)) as f:
                org_note = f.read()
            #Drop everything before the first header (startup options, etc)
            org_note = org_note[org_note.find('*'):]
            with open(paper_dir+'/literature.org','r+') as f:
                master_org = f.read()
                #Make use of the fact that we know the bib key to find
                #and replace this specific entry. The |$ is because it
                #could be the last entry, in which case there's not
                #another one after it.
                re_sub = re.subn('(.*)[*] [A-Z]* .* \t+.*:BIBTEX-KEY: %s.*?(\n[*] [A-Z]*|$)'%bib_id,r"\1"+org_note+'\n'+r"\2",master_org,flags=re.DOTALL)
                assert re_sub[1]==1,'Thought org file was updated but no (or more than one) substitution made...'
                f.seek(0)
                f.write(re_sub[0])
                f.truncate()
        if os.path.getmtime('%s/%s.bib'%(dir_path,bib_id)) > modified_time['bib']:
            print('Bib file %s.bib updated, copying new changes to master bib'%bib_id)
            with open('%s/%s.bib'%(dir_path,bib_id)) as f:
                bib_file = f.read()
            with open(paper_dir+'/literature.bib','r+') as f:
                master_bib = f.read()
                re_sub = re.subn('@[a-zA-z]*{%s,.*(@|$)'%bib_id,bib_file+r"\1",master_bib,flags=re.DOTALL)
                assert re_sub[1]==1,'Thought bib file was updated but no (or more than one) substitution made...'
                f.seek(0)
                f.write(re_sub[0])
                f.truncate()
            
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
    
if __name__ == '__main__':
    update()
