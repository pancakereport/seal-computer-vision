FROM nvcr.io/nvidia/tensorflow:20.09-tf2-py3

RUN pip3 install --upgrade pip

RUN pip3 install jupyter

RUN pip3 install jupyterlab

RUN pip3 install keras==2.4.0

RUN pip3 install tensorflow==2.4.0

RUN pip3 install keras_retinanet

RUN pip3 install matplotlib

RUN pip3 install numpy

RUN pip3 install progressbar2

RUN pip3 install Pillow

RUN pip3 install pandas

RUN apt-get update

RUN apt-get -y update

RUN apt -y install git

RUN apt-get -y install wget

ARG DEBIAN_FRONTEND="noninteractive" 

RUN apt-get -y install python3-opencv

RUN git clone https://github.com/fizyr/keras-retinanet.git

RUN git clone https://github.com/martinzlocha/anchor-optimization.git

RUN git clone https://github.com/pancakereport/seal-computer-vision.git

RUN wget https://github.com/fizyr/keras-retinanet/releases/download/0.5.1/resnet50_coco_best_v2.1.0.h5

RUN wget -P harbor_seal_vm https://harbor-seal-models.s3.amazonaws.com/model_no_other.h5

RUN wget -P harbor_seal_vm https://harbor-seal-models.s3.amazonaws.com/model_other.h5

RUN wget -P mids-251-elephant-seal https://elephant-seal-models.s3.amazonaws.com/model_no_other.h5

RUN wget -P mids-251-elephant-seal https://elephant-seal-models.s3.amazonaws.com/model_other.h5

CMD ["python3", "keras-retinanet/setup.py", "build_ext", "--inplace"]
