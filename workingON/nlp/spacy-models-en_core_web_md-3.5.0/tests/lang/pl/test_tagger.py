import pytest
from spacy.symbols import SPACE


def test_pl_tagger_spaces(NLP):
    """Ensure spaces are assigned the POS tag SPACE"""
    doc = NLP("Some\nspaces are\tnecessary.")
    assert doc[0].pos != SPACE
    assert doc[0].pos_ != "SPACE"
    assert doc[1].pos == SPACE
    assert doc[1].pos_ == "SPACE"
    assert doc[1].tag_ == "_SP"
    assert doc[2].pos != SPACE
    assert doc[3].pos != SPACE
    assert doc[4].pos == SPACE


def test_pl_tagger_return_char(NLP):
    """Ensure spaces are assigned the POS tag SPACE"""
    text = (
        "hi Aaron,\r\n\r\nHow is your schedule today, I was wondering if "
        "you had time for a phone\r\ncall this afternoon?\r\n\r\n\r\n"
    )
    doc = NLP(text)
    for token in doc:
        if token.is_space:
            assert token.pos == SPACE
    assert doc[3].text == "\r\n\r\n"
    assert doc[3].is_space
    assert doc[3].pos == SPACE


@pytest.mark.xfail
def test_pl_tagger_lemma_doc(NLP):
    doc = NLP("był", disable=["lemmatizer"])
    doc[0].tag_ = "PRAET"
    lemmatizer = NLP.get_pipe("lemmatizer")
    doc = lemmatizer(doc)
    assert doc[0].lemma_ == "być"


def test_pl_tagger_lemma_assignment(NLP):
    doc = NLP("Poczuł przyjemną woń mocnej kawy.")
    assert all(t.lemma_ != "" for t in doc)