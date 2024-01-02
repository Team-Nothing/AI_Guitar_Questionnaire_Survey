import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from PIL import Image
from opencc import OpenCC
from snownlp import SnowNLP
from ckiptagger import WS, POS
from scipy.ndimage import gaussian_gradient_magnitude
from wordcloud import WordCloud, ImageColorGenerator

ws = WS("./model/data")
pos = POS("./model/data")
cc = OpenCC('t2s')

df = pd.read_csv("data.csv")

texts_df = df["以上所有服務的任何想法"].dropna()

positive = []
neutral = []
negative = []
    
def sentiment_classify():    
    for index, text in texts_df.items():
        s = SnowNLP(cc.convert(text))
        point = round(s.sentiments, 2)
        if point > 0.6:
            # positive.append(f"{point}: {text}")
            positive.append(text)
        elif s.sentiments >= 0.4:
            # neutral.append(f"{point}: {text}")
            neutral.append(text)
        else:
            # negative.append(f"{point}: {text}")
            negative.append(text)
    
    # print(positive)
    # print(neutral)
    # print(negative)

def draw_pie_chart():
    positive_count = len(positive)
    neutral_count = len(neutral)
    negative_count = len(negative)

    labels = ['Positive', 'Neutral', 'Negative']
    sizes = [positive_count, neutral_count, negative_count]
    colors = ['lightgreen', 'lightblue', 'lightcoral']

    plt.pie(sizes, labels=labels, colors=colors, autopct='%1.1f%%', startangle=90)
    plt.axis('equal')
    plt.title('')
    plt.show()

def draw_word_cloud(texts, filter_words = [], mask_path = None, save_name = None):
    words = []
    
    for i in range(len(texts)):
        words_list = ws([texts[i]])[0]
        words_list_pos = pos(words_list)
        
        for j in range(len(words_list_pos)):
            if ('A' in words_list_pos[j]) or ('Na' in words_list_pos[j]) or ('Nb' in words_list_pos[j]):
                words.append(words_list[j])
                
    print(words)
    content = ' '.join(words)

    if mask_path == None:
        wc = WordCloud(
            font_path = "./font/msjh.ttc",
            width = 800, 
            height = 600,
            stopwords = filter_words
        )
        
        if save_name != None:
            wc.to_file(save_name)
            
        cloud = wc.generate(content)
        
        plt.imshow(cloud)
        plt.axis("off")
        plt.show()
    else:
        mask_color = np.array(Image.open(mask_path))
        mask_color = mask_color[::3, ::3]
        
        mask = mask_color.copy()
        mask[mask.sum(axis=2) == 0] = 255
        
        edges = np.mean([gaussian_gradient_magnitude(mask_color[:, :, i] / 255., 2) for i in range(3)], axis = 0)
        mask[edges > .08] = 255
        
        wc = WordCloud(
            font_path = "./font/msjh.ttc",
            mask = mask,
            stopwords = filter_words,
            relative_scaling = 0
        )
        
        cloud = wc.generate(content)
        # plt.imshow(wc)
        
        cloud_colors = ImageColorGenerator(mask_color)
        wc.recolor(color_func = cloud_colors)
        # plt.figure(figsize=(10, 10))
        plt.imshow(wc, interpolation="bilinear")
        
        if save_name != None:
            wc.to_file(save_name)
        
        # plt.figure(figsize=(10, 10))
        # plt.title("Original Image")
        # plt.imshow(mask_color)

        # plt.figure(figsize=(10, 10))
        # plt.title("Edge map")
        # plt.imshow(edges)
        plt.axis("off")
        plt.show()
            
if __name__ == "__main__":
    sentiment_classify()
    # draw_pie_chart()
    # draw_word_cloud(positive,
    #                 filter_words = ['問卷', '鋁箔包', '\n窩','飲料\n'],
    #                 mask_path = "./mask_img/positive.png")
    # draw_word_cloud(neutral, 
    #                 filter_words = ['問卷', '人', '者', '螢幕', '螢幕'],
    #                 mask_path = "./mask_img/neutral.png")
    draw_word_cloud(negative)
    
    