from __future__ import division, unicode_literals
from bs4 import BeautifulSoup
import requests
import re
import time
import codecs
import os

from pymongo import MongoClient
import http.client, urllib.parse



def connecting_mango():

    #### connecting to client ####
    client = MongoClient("localhost", 27017)

    #### accessing or creating the data base
    yelp = client.Yelp


    #### deletes the collection since we are testing our code adn creating and recreating the collection ####
    #donut = yelp['sf_donut_shops']
    #donut.drop()


    #### creating a collection since we are testing our code and hence, deleting and creating connections ####
    yelp.create_collection('sf_donut_shops')
    return(yelp)


def yelp_scrap(path):

    """
    Read the yelp pages with top 40 donut shops.
    We don't need to check if the the shop is actually selling the donuts.
    """

    time.sleep(5)
    count = 1

    while (count < 5):
        url = "https://www.yelp.com/search?find_desc=donut+shop&find_loc=San+Francisco%2C+CA+94105&start=" + str(count*10 - 10)
        r = requests.get(url ,headers=header)
        soup = BeautifulSoup(r.text, 'lxml')

        name = path + "sf_donut_shop_search_page_" + str(count) + ".htm"
        with open(name, "w") as file:
            file.write(str(soup))

        time.sleep(5)
        count += 1


def yelp_parse(path, yelp):

    """
    Read the previously saved html file
    and parse out important information.
    """

    short_url = 'https://www.yelp.com'
    count = 1
    while count < 5:

        filename = path + "sf_donut_shop_search_page_" + str(count) + ".htm"
        f = codecs.open(filename, 'r', 'utf-8')
        document = BeautifulSoup(f.read(), 'lxml')

        list_document = document.findAll('div', attrs={'class': "container__09f24__mpR8_ hoverable__09f24__wQ_on margin-t3__09f24__riq4X margin-b3__09f24__l9v5d padding-t3__09f24__TMrIW padding-r3__09f24__eaF7p padding-b3__09f24__S8R2d padding-l3__09f24__IOjKY border--top__09f24__exYYb border--right__09f24__X7Tln border--bottom__09f24___mg5X border--left__09f24__DMOkM border-color--default__09f24__NPAKY"})
        flag = 0



        for kt in list_document:

            if flag in range(2, 12):

                #### restuarant name and rank####

                part_doc = kt.find('span', attrs={'class': 'css-1uq0cfn'})

                name = re.search(r"([\d]+).\s(.+)", part_doc.text).group(2)
                yelp.sf_donut_shops.insert_one({'Name': name})

                rank = re.search(r"([\d]+).\s(.+)",part_doc.text).group(1)
                yelp.sf_donut_shops.update_one({'Name':name}, [{'$set': {'Rank': rank}}])

                part_doc = kt.findAll('button', attrs={'class':'css-un1vt9'})

                #### categories assigned to the restaurants ####

                categ = []

                try:
                    for kk1 in part_doc:
                        categ.append(kk1.text)
                        print(kk1.text)
                    yelp.sf_donut_shops.update_one({'Name':name}, [{'$set': {'Categories': categ}}])
                except:
                    pass

                #### number of reviews given ####

                part_doc = kt.find('span', attrs={'class':'reviewCount__09f24__tnBk4 css-1e4fdj9'})
                num_reviews = int(part_doc.text)
                yelp.sf_donut_shops.update_one({'Name':name}, [{'$set': {'Num_Reviews': num_reviews}}])


                #### average rating ####

                part_doc = kt.find('span', attrs={'class':'display--inline__09f24__c6N_k border-color--default__09f24__NPAKY'})
                avg_rating = part_doc.div['aria-label']


                yelp.sf_donut_shops.update_one({'Name':name}, [{'$set': {'Avg_Rating': avg_rating}}])

                ### finding the variables for the true or false earlier ###

                part_doc = kt.findAll('span', attrs={'class':'icon--16-checkmark-v2 css-1z0vsn7'})
                try:
                    for ii in part_doc:
                        print(ii['aria-hidden'])
                except:
                    pass



                ### true or false for whether they take out is there or not
                ### and other variables

                part_doc = kt.findAll('span', attrs={'class':' raw__09f24__T4Ezm'})

                for jj in part_doc:
                    print(jj.text)

                ### finding the variables for the true or false earlier ###

                #### number of dollar signs ####

                try:
                    part_doc = kt.find('span', attrs={'class':'priceRange__09f24__mmOuH css-18qxe2r'})
                    yelp.sf_donut_shops.update_one({'Name': name}, [{'$set': {'ExpenseRange': len(list(part_doc.text))}}])
                    print(len(list(part_doc.text)))

                except:
                    pass

                #### whether start - order is there or not ####

                part_doc = kt.findAll('a', attrs={'class':'platformSearchAction__09f24__PNDZS horizontalSearchAction__09f24__yYw_h css-1qdoc3r'})
                for kk in part_doc:

                    yelp.sf_donut_shops.update_one({'Name': name}, [{'$set': {'StartButton': kk['data-button']}}])
                    print(kk['data-button'])


                #### page-url ####

                part_doc = kt.findAll('a', attrs = {'class':'css-1422juy'})

                try:
                    for kj in part_doc:
                        url = short_url + kj['href']
                        yelp.sf_donut_shops.update_one({'Name': name}, [{'$set': {'URL': url}}])
                        print(url)

                except:
                    pass
            flag += 1

        count += 1


def yelp_page_store(shop_path, yelp):
    time.sleep(5)
    count = 1

    print(yelp.stats)

    for x in yelp.sf_donut_shops.find({}, {"URL": 1}):
        try:
            link = x.get('URL')
            print(link)
            r = requests.get(link, headers=header)
            soup = BeautifulSoup(r.text, 'lxml')

            name = shop_path + "sf_donut_shop_" + str(count) + ".htm"
            with open(name, "w") as file:
                file.write(str(soup))

            time.sleep(5)
            count += 1

        except:
            time.sleep(5)
            pass


def address_lat_lag(adds):
    conn = http.client.HTTPConnection('api.positionstack.com')

    params = urllib.parse.urlencode({
    'access_key': 'a0bdad8da570f9a6921c47ae1c8f770b',
    'query': adds,
    'region': 'San Francisco',
    'limit': 1,
})

    conn.request('GET', '/v1/forward?{}'.format(params))

    res = conn.getresponse()
    data = res.read()

    print(data.decode('utf-8'))
    return (data.decode('utf-8'))

def donut_parser(shop_path, yelp):
    os.chdir(shop_path)
    count = 1

    while count < 41:
        try:
            filename = "sf_donut_shop_" + str(count) + ".htm"
            f= codecs.open(filename, 'r', 'utf-8')
            document = BeautifulSoup(f.read(), 'lxml')

            #### calculating the name ###

            part_doc = document.find('h1', attrs={'class': 'css-12dgwvn'})
            name = part_doc.text

            #### extracting the address ####

            part_doc = document.find('p', attrs={'class': 'css-qyp8bo'})
            address = part_doc.text

            #### caculating lat longitude ####

            geo_address = address_lat_lag(address)

            yelp.sf_donut_shops.update_one({'Name': name}, [{'$set': {'Address': address}}])
            yelp.sf_donut_shops.update_one({'Name': name}, [{'$set': {'Geo_Address': geo_address}}])

            part_doc = document.find('div', attrs={'class': 'css-xp8w2v padding-t2__09f24__Y6duA padding-r2__09f24__ByXi4 padding-b2__09f24__F0z5y padding-l2__09f24__kf_t_ border--top__09f24__exYYb border--right__09f24__X7Tln border--bottom__09f24___mg5X border--left__09f24__DMOkM border-radius--regular__09f24__MLlCO background-color--white__09f24__ulvSM'})
            phone_number = part_doc.findAll('p', attrs={'class':'css-1p9ibgf'})
            data = list(i.text for i in phone_number)

            phone = "N/A"
            shop_link = "N/A"

            for i in data:
                if "415" in i:
                    phone = i
                elif "https" in i:
                    shop_link = i

            yelp.sf_donut_shops.update_one({'Name': name}, [{'$set': {'Phone': phone}}])
            yelp.sf_donut_shops.update_one({'Name': name}, [{'$set': {'Shop Link': shop_link}}])
        except:
            pass

        count += 1



if __name__ == "__main__":
    path = '/Users/rctrj/Downloads/422/htm_files/'
    shop_path = '/Users/rctrj/Downloads/422/shop_htm_files/'

    header = {
        "user-agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/74.0.3729.169 Safari/537.36" ,
        "authority": "www.tagesschau.de",
        "method": "GET",
        "path":"/",
        "scheme":"https",
        "accept": "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9",
        "accept-encoding": "gzip, deflate, br",
        "accept-language": "en-US,en;q=0.9,de;q=0.8",
        "cache-control": "max-age=0",
        "cookie": "atuserid=%7B%22name%22%3A%22atuserid%22%2C%22val%22%3A%2257ea5dd6-4c35-4982-942f-8a7f8b8c3a4b%22%2C%22options%22%3A%7B%22end%22%3A%222023-02-17T05%3A02%3A59.936Z%22%2C%22path%22%3A%22%2F%22%7D%7D; atidvisitor=%7B%22name%22%3A%22atidvisitor%22%2C%22val%22%3A%7B%22vrn%22%3A%22-595936-%22%7D%2C%22options%22%3A%7B%22path%22%3A%22%2F%22%2C%22session%22%3A15724800%2C%22end%22%3A15724800%7D%7D",
        "sec-ch-ua-mobile": "?0",
        "sec-ch-ua-platform": "Windows",
        "sec-fetch-dest": "document",
        "sec-fetch-mode": "navigate",
        "sec-fetch-site": "none",
        "sec-fetch-user": "?1",
        "upgrade-insecure-requests": "1"
    }


    yelp_scrap(path)
    yelp = connecting_mango()
    yelp_parse(path, yelp)
    yelp_page_store(shop_path, yelp)
    donut_parser(shop_path, yelp)




