// Faith: implementing sort by methods 26/03

// sort data alphabetically
class alphabeticalComp implements Comparator <DATACLASS> {
  @Override
    int compare(DATACLASS obj1, DATACLASS obj2) {
    return obj1.getname().compareTo(obj2.getname());
  }
}

// sort data by mass
class massCompare implements Comparator <DATACLASS> {
  @Override
    int compare(DATACLASS obj1, DATACLASS obj2) {
    return obj1.getMass().compareTo(obj2.getMass());
  }
}

// sort data by diamater
class diameterComp implements Comparator <DATACLASS> {
  @Override
    int compare(DATACLASS obj1, DATACLASS obj2) {
    return obj1.getDiameter().compareTo(obj2.getDiameter());
  }
}
