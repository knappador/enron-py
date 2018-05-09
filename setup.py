from setuptools import setup, find_packages


setup(name="enron",
      version="0.1.dev2",
      description="Type foundation for building strict accounting systems",
      keywords="bookkeeping accounting",
      url="https://github.com/knappador/enron-py",
      author="Brian Knapp",
      author_email="knappador@gmail.com",
      license="BSD 4-clause",
      zip_safe=False,
      packages=find_packages(exclude=["tests*"]),
      python_requires=">=3",
      test_suite="nose.collector",
      tests_require=["nose"],

      classifiers=[
          "Development Status :: 3 - Alpha",
          "License :: OSI Approved :: BSD License",
          "Programming Language :: Python :: 3",
          "Intended Audience :: Financial and Insurance Industry",
          "Topic :: Office/Business :: Financial :: Accounting",
          "Topic :: Documentation :: Sphinx",
          "Topic :: Software Development :: Libraries",
          "Natural Language :: English",
        ],)
