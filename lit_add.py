#!/usr/bin/env python

#Directory that all your papers will be kept in. Master bib and org
#files will be saved directly in this directory (as literature.bib and
#literature.org, respectively), each reference will be given a
#sub-directory (the bibtex key will be its name), and mendeley_to_org
#will look for the directory Unsorted within this directory for
#previously annotated files.
paper_dir = "~/Dropbox/Docs/Papers"

#How you want your org note formatted. I haven't played around with
#this too much, so it probably isn't very robust. If you want to add
#or remove fields, that's on you.
org_format=u"""#+STARTUP: showall
* {todo} {title} \t\t\t\t\t {tags}
   :PROPERTIES:
   :ADDED: [{date}]
   :BIBTEX-KEY: {key}
   :AUTHOR: {authors}
   :YEAR: {year}
   :PUBLICATION: {publication}
   :END:

** Keywords
   {kws}

** Notes
   {notes}

** Annotations
   {annotations}

** Links
   PDF: [[file:{pdf_path}]]
   BibTex: [[file:{bib_path}]]
   Notes: [[file:{org_path}]]
"""

#Find way to bind into emacs

def add_pdf(pdf_path):
    import poppler,re,os,pybib_utils,bibtexparser,time
    
    if '~' in pdf_path:
        pdf_path = os.path.expanduser(pdf_path)
    pdf_path = os.path.abspath(pdf_path)
    
    #From
    #https://stackoverflow.com/questions/27910/finding-a-doi-in-a-document-or-page/10324802#10324802,
    #this is the best regex to find a doi in a page.
    doi_regex=re.compile('\\b(10[.][0-9]{4,}(?:[.][0-9]+)*/(?:(?![\"&\'<>])\S)+)\\b')

    doc = poppler.document_new_from_file('file://'+pdf_path,None)
    pages = [doc.get_page(i) for i in range(doc.get_n_pages())]
    doi = []
    for no,page in enumerate(pages):
        text = page.get_text()
        doi += re.findall(doi_regex,text)
    #There may be multiple dois in the document, but we assume the one
    #for this document is the one that shows up the most.
    try:
        doi = max(set(doi),key=doi.count)
    except ValueError:
        raise Exception("Unable to find doi in pdf %s, you'll have to manually download bibtex"%pdf_path)
    
    print('Checking doi %s'%doi)
    bib = pybib_utils.get_bibtex(doi)
    bib = bibtexparser.loads(bib).entries[0]
    bib['ID'] = bib['ID'].replace('_','')

    with open(paper_dir+'/literature.bib') as f:
        bib_str = f.read().decode('utf8')
        master_bib = bibtexparser.loads(bib_str)

    if bib['ID'] in master_bib.entries_dict:
        print 'File with bib id %s already in master bib file, skipping'%bib['ID']
    else:
        bib_save,bib_path = setup_folders_withfile(pdf_path,bib)
        
        master_org_add(bib_path)
        master_bib_add(bib_save)
    
def add_bib(bib_path):
    import bibtexparser,os
    
    if '~' in bib_path:
        bib_path = os.path.expanduser(bib_path)
    bib_path = os.path.abspath(bib_path)

    with open(bib_path) as f:
        bib_str = f.read().decode('utf8')
        bib_db = bibtexparser.loads(bib_str)
        
    with open(paper_dir+'/literature.bib') as f:
        bib_str = f.read().decode('utf8')
        master_bib = bibtexparser.loads(bib_str)
        
    for bib in bib_db.entries:
        if bib['ID'] in master_bib.entries_dict:
            print('Bib %s already in master bib file, skipping'%bib['ID'])
            continue
        try:
            if os.path.isfile(os.path.expanduser(bib['file'])):
                print 'File %s found for bib %s from %s'%(bib['file'],bib['ID'],bib_path)
                file_path = bib['file']
                if '~' in file_path:
                    file_path = os.path.expanduser(file_path)
                bib_save,bib_path = setup_folders_withfile(file_path,bib)
            else:
                print 'File %s for bib %s from %s not found!'%(bib['file'],bib['ID'],bib_path)
                bib.pop('file')
                bib_save,bib_path = setup_folders_nofile(bib)
        except KeyError as e:
            print 'No file field found in bib %s from %s'%(bib['ID'],bib_path)
            bib_save,bib_path = setup_folders_nofile(bib)
        master_org_add(bib_path)
        master_bib_add(bib_save)
        
def setup_folders_nofile(bib):
    import bibtexparser,os,time
    
    bib_path = paper_dir+'/{bib_id}/{bib_id}.bib'.format(bib_id=bib['ID'])
    org_path = os.path.splitext(bib_path)[0]+'.org'
    
    os.makedirs(os.path.split(org_path)[0])

    #This allows us to replace the home directory with a tilde,
    #since I'll be using this across computers. Org can handle the
    #tilde without any problem, so it's alright
    for tmp_path in ['org_path','bib_path']:
        exec(tmp_path+" = "+tmp_path+".replace(os.path.expanduser('~'),'~')")
        
    try:
        if bib['ENTRYTYPE']=='article':
            pub = bib['journal']
        elif bib['ENTRYTYPE']=='misc':
            pub = bib['organization']
        elif bib['ENTRYTYPE']=='book':
            pub = bib['publisher']
        else:
            pub = u''
    except KeyError:
        pub = u''
        
    org_file = org_format.format(title=bib['title'],tags='',date=time.strftime("%Y-%m-%d"),annotations='',notes='',pdf_path='',bib_path=bib_path,todo='TODO',org_path=org_path,kws='',authors=bib['author'],year=bib['year'],publication=pub,key=bib['ID'])
    
    bib_save = bibtexparser.bibdatabase.BibDatabase()
    bib_save.entries = [bib]
    
    #expanduser is necessary because python does not like the
    #tilde; we need to expand it to get a path it can use.
    if not os.path.isfile(os.path.expanduser(bib_path)):
        with open(os.path.expanduser(bib_path),'w') as f:
            bib_str = bibtexparser.dumps(bib_save)
            f.write(bib_str.encode('utf8'))
    else:
        print('Found a bib file at %s, not saving new one'%bib_path)
    if not os.path.isfile(os.path.expanduser(org_path)):
        with open(os.path.expanduser(org_path),'w') as f:
            f.write(org_file.encode('utf8'))
    else:
        print('Found an org file at %s, not saving new one'%org_path)

    print('Added entry (no file) for bib_id %s, check its folder to make sure everything looks like you want it to'%bib['ID'])        
    
    return bib_save,bib_path
        
def setup_folders_withfile(file_path,bib):
    import bibtexparser,os,time
    
    new_path = paper_dir+'/{bib_id}/{bib_id}{extension}'.format(bib_id=bib['ID'],extension=os.path.splitext(file_path)[1])
    bib_path = os.path.splitext(new_path)[0]+'.bib'
    org_path = os.path.splitext(new_path)[0]+'.org'

    os.renames(file_path,new_path)
    
    #This allows us to replace the home directory with a tilde,
    #since I'll be using this across computers. Org can handle the
    #tilde without any problem, so it's alright
    for tmp_path in ['new_path','org_path','bib_path']:
        exec(tmp_path+" = "+tmp_path+".replace(os.path.expanduser('~'),'~')")

    try:
        if bib['ENTRYTYPE']=='article':
            pub = bib['journal']
        elif bib['ENTRYTYPE']=='misc':
            pub = bib['organization']
        elif bib['ENTRYTYPE']=='book':
            pub = bib['publisher']
        else:
            pub = u''
    except KeyError:
        pub = u''
    
    org_file = org_format.format(title=bib['title'],tags='',date=time.strftime("%Y-%m-%d").encode('utf8'),annotations=u'',notes=u'',pdf_path=new_path.encode('utf8'),bib_path=bib_path.encode('utf8'),todo=u'TODO',org_path=org_path.encode('utf8'),kws=u'',authors=bib['author'],year=bib['year'],publication=pub,key=bib['ID'])
    
    bib['file'] = new_path
    bib_save = bibtexparser.bibdatabase.BibDatabase()
    bib_save.entries = [bib]
    
    #expanduser is necessary because python does not like the
    #tilde; we need to expand it to get a path it can use.
    if not os.path.isfile(os.path.expanduser(bib_path)):    
        with open(os.path.expanduser(bib_path),'w') as f:
            bib_str = bibtexparser.dumps(bib_save)
            f.write(bib_str.encode('utf8'))
    else:
        print('Found a bib file at %s, not saving new one'%bib_path)
    if not os.path.isfile(os.path.expanduser(org_path)):            
        with open(os.path.expanduser(org_path),'w') as f:
            f.write(org_file.encode('utf8'))
    else:
        print('Found an org file at %s, not saving new one'%org_path)

    print('Added entry (with file) for bib_id %s, check its folder to make sure everything looks like you want it to'%bib['ID'])
    
    return bib_save,bib_path
    
def master_bib_add(bib):
    import bibtexparser,os,stat

    os.chmod(paper_dir+'/literature.bib',stat.S_IWUSR|stat.S_IREAD)
    
    with open(paper_dir+'/literature.bib') as f:
        master_bib_str = f.read().decode('utf8')
        
    master_bib_str += bibtexparser.dumps(bib)
    master_bib = bibtexparser.loads(master_bib_str)
    
    #bibtexparser.dumps automatically sorts by bib id. I have to get
    #the string and then save it this way because bibtexparser's
    #writer tries to write it as ascii. Encoding it manually as utf8
    #allows it to go without any problems.
    with open(paper_dir+'/literature.bib','w') as f:
        master_bib_str = bibtexparser.dumps(master_bib)
        f.write(master_bib_str.encode('utf8'))

    os.chmod(paper_dir+'/literature.bib',stat.S_IREAD|stat.S_IRGRP|stat.S_IROTH)
    
def master_org_add(bib_path):
    import re,os,stat
    from lit_update import key_get
    
    org_path = os.path.splitext(bib_path)[0]+'.org'

    os.chmod(paper_dir+'/literature.org',stat.S_IWUSR|stat.S_IREAD)
    
    #Add new org to master org, sort by keys
    with open(paper_dir+'/literature.org') as f:
        master_org = f.read().decode('utf8')
        
    #Splits into org todo keywords and entries
    #First entry is '', so we drop it.
    master_org = re.split('^[*] ([A-Z]+)',master_org,flags=re.MULTILINE)[1:]
    master_org = [(i,j,key_get(j)) for i,j in zip(master_org[::2],master_org[1::2])]

    with open(os.path.expanduser(org_path)) as f:
        new_org = f.read().decode('utf8')+'\n\n'
    new_org = new_org[new_org.find('*'):]
    new_org = re.split('^[*] ([A-Z]+)',new_org,flags=re.MULTILINE)[1:]
    new_org = [(new_org[0],new_org[1],key_get(new_org[1]))]
    
    master_org+=new_org
    master_org.sort(key=lambda x: x[2].lower())
    master_org = '* '.join(['']+[i+j for i,j,k in master_org])

    with open(paper_dir+'/literature.org','w') as f:
        f.write(master_org.encode('utf8'))

    os.chmod(paper_dir+'/literature.org',stat.S_IREAD|stat.S_IRGRP|stat.S_IROTH)

    
if __name__ == '__main__':
    import sys,os,lit_update
    paper_dir = os.path.expanduser(paper_dir)
    if os.path.splitext(sys.argv[1])[1] == '.bib':
        add_bib(sys.argv[1])
    elif os.path.splitext(sys.argv[1])[1] == '.pdf':
        add_pdf(sys.argv[1])
    else:
        raise Exception('Don\'t know how to deal with extension %s'%os.path.splitext(sys.argv[1])[1])

