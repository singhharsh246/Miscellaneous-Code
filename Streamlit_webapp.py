import streamlit as st
import pandas as pd
import numpy as np

st.markdown("# Harsh Harsh")
st.markdown("## Graduate Research Analyst")
st.markdown("### Looking for full-time positions starting July 2022")
st.markdown("""
    I write articles about Data Science, Python and related topics. 
    The articles are mostly written on the Medium platform.
    
    You can find my articles [here](https://medium.com/@singhharsh246)
    and if you would like to know when I publish new ones, you can 
    sign up for an email alert on my Medium 
    [page](https://medium.com/@singhharsh246/subscribe).
    Below are a few articles you might find interesting...
""")

with st.container():
    image_col, text_col = st.columns((1,2))
    with image_col:
        st.image("https://miro.medium.com/max/1400/0*5zaQ0qT76_AD0EN6")

    with text_col:
        st.subheader("Application of SHAP (SHapley Value exPression")
        st.write("""Easy to interpret graphs used in calculating importance of features in ML model
            """)
        st.markdown("[Read more...](https://singhharsh246.medium.com/shap-in-python-35d0297d8285)")

with st.container():
    image_col, text_col = st.columns((1,2))
    with image_col:
        st.image("https://miro.medium.com/max/1400/0*CuJ9XGy_WIr6FUv7")

    with text_col:
        st.subheader("Rational UI Design with Streamlit")
        st.write("""
            A full machine learning case study from start to end. 
            I have tried to cover all the basic steps imvolved while 
            approaching a Machine Learning case study.""")
        st.markdown("[Read more...](https://singhharsh246.medium.com/machine-learning-project-4cdaaca14143)")